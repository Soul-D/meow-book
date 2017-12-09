package chap8

import cats.{Applicative, Id}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object TestingAsync extends App{

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {

  }

  trait TestUptimeClient extends UptimeClient[Id] {

  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hosts: List[String]): F[Int] =
      hosts.traverse(client.getUptime).map(_.sum)
  }

  def testUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 20)
    val client: UptimeClient[Id] = new TestUptimeClient {
      def getUptime(hostname: String) = hosts(hostname)
    }
    val underTest: UptimeService[Id] = new UptimeService[Id](client)

    println(underTest.getTotalUptime(hosts.keys.toList) == 30)
  }

  testUptime()
}
