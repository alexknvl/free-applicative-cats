object Main {
  import scala.language.higherKinds
  import scala.util.Try
  import cats._, free._, syntax.all._
  import cats.std.option._
  import cats.std.list._
  import cats.data._
  import free.{FreeApplicative => FA}

  trait Curry[F] {
    type Argument
    type Result
    def apply(f: F)(a: Argument): Result
  }

  object Curry {
    def apply[F](implicit ev: Curry[F]): Aux[F, ev.Argument, ev.Result] = ev

    type Aux[F, A, R] = Curry[F] {
      type Argument = A
      type Result = R
    }

    implicit def func1[A, R]: Aux[A => R, A, R] = new Curry[A => R] {
      override type Argument = A
      override type Result = R
      override def apply(f: A => R)(a: A): R =
        f(a)
    }
    implicit def func2[A, B, R]: Aux[(A, B) => R, A, B => R] = new Curry[(A, B) => R] {
      override type Argument = A
      override type Result = B => R
      override def apply(f: (A, B) => R)(a: A): B => R =
        b => f(a, b)
    }
    implicit def func3[A, B, C, R]: Aux[(A, B, C) => R, A, (B, C) => R] = new Curry[(A, B, C) => R] {
      override type Argument = A
      override type Result = (B, C) => R
      override def apply(f: (A, B, C) => R)(a: A): (B, C) => R =
        (b, c) => f(a, b, c)
    }
  }

  implicit class SugarOps[F[_], G](val fg: FA[F, G]) {
    def <*>[A, R]
    (a: FA[F, A])
    (implicit curry: Curry.Aux[G, A, R]): FA[F, R] = {
      a.ap(fg.map(curry.apply))
    }
  }

  case class User(userName: String, fullName: String, id: Int)
  object User {
    implicit object UserShow extends Show[User] {
      override def show(f: User): String = f.toString
    }
  }

  // renamed to Opt to avoid clashing with Option
  case class Opt[A](name: String, default: Option[A], reader: String => Option[A])
  object Opt {
    implicit object OptFunctor extends Functor[Opt] {
      override def map[A, B](fa: Opt[A])(f: (A) => B): Opt[B] =
        Opt(
          name = fa.name,
          default = fa.default.map(f),
          reader = fa.reader.andThen(_.map(f))
        )
    }
  }

  def one[A](o: Opt[A]): FA[Opt, A] = FA.lift(o)
  def readInt(s: String): Option[Int] = Try(s.toInt).toOption
  val userP: FA[Opt, User] =
    FA.pure(User.apply _) <*>
      one(Opt("username", None, Some.apply[String])) <*>
      one(Opt("fullname", Some(""), Some.apply[String])) <*>
      one(Opt("id", None, readInt))

  def parserDefault[A](a: FA[Opt, A]): Option[A] = a.run(new ~>[Opt, Option] {
    def apply[B](b: Opt[B]): Option[B] = b.default
  })
  def allOptions[A](a: FA[Opt, A]): List[String] = {
    type G[B] = Const[List[String], B]
    a.run(new ~>[Opt, G] {
      def apply[B](b: Opt[B]): G[B] = Const.apply(List(b.name))
    }).getConst
  }
  def matchOpt[A](opt: String, value: String, fa: FA[Opt, A]): Option[FA[Opt, A]] = {
    fa match {
      case FA.Pure(_) => None
      case ap: FA.Ap[Opt, A] if "--" + ap.pivot.name == opt =>
        ap.pivot.reader(value).map(FA.Pure(_).ap(ap.fn))
      case x: FA.Ap[Opt, A] =>
        matchOpt(opt, value, x.fn).map { FA.lift(x.pivot).ap }
    }
  }
  def runParser[A](p: FA[Opt, A], args: List[String]): Option[A] = args match {
    case opt :: value :: rest => matchOpt(opt, value, p).flatMap(runParser(_, rest))
    case Nil => parserDefault(p)
    case _ => None
  }

  def main(args: Array[String]): Unit = {
    println(parserDefault(userP))
    println(allOptions(userP))

    println(runParser(userP, Nil))
    println(runParser(userP, List(
      "--")))
    println(runParser(userP, List(
      "--blah", "1")))
    println(runParser(userP, List(
      "--username", "alexknvl")))

    println(runParser(userP, List(
      "--username", "alexknvl",
      "--id", "0")))

    println(runParser(userP, List(
      "--username", "alexknvl",
      "--fullname", "test",
      "--id", "0")))
  }
}
