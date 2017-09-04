trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt
}

val booleans = for(i <- integers) yield i > 0

trait Tree

case class Inner(l: Tree, r: Tree) extends Tree

case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for(i <- integers) yield Leaf(i)

def inners: Generator[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l, r)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if(isLeaf) leafs else inners
} yield tree

trees.generate

List(4).flatMap(x => List(x * 2))