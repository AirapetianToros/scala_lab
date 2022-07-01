import Set._
import Set.*

class MySuite extends munit.FunSuite {
  test("union of two nonempty sets") {
    val unionSet: Set[Int] = union(Set(1, 3, 5), Set(2, 3))
    val actual = areEqual(unionSet, Set(1, 3, 5, 2))
    assertEquals(actual, true)
  }

  test("union with empty set") {
    val unionSet: Set[Int] = union(Set(1, 3, 5), Set())
    val actual = areEqual(unionSet, Set(1, 3, 5))
    assertEquals(actual, true)
  }

  test("union of two empty sets") {
    val unionSet: Set[Int] = union(Set(), Set())
    val actual = areEqual(unionSet, Set())
    assertEquals(actual, true)
  }

  test("difference of two nonempty sets") {
    val differenceSet: Set[Int] = difference(Set(1, 3, 5), Set(1, 4))
    val actual = areEqual(differenceSet, Set(3, 5))
    assertEquals(actual, true)
  }

  test("differece of two empty sets") {
    val differenceSet: Set[Int] = difference(Set(), Set())
    val actual = areEqual(differenceSet, Set())
    assertEquals(actual, true)
  }

  test("differce of set and its superset") {
    val differenceSet: Set[Int] = difference(Set(1, 3, 5), Set(1, 3, 5, 2, 4))
    val actual = areEqual(differenceSet, Set())
    assertEquals(actual, true)
  }

  test("unionBy") {
    val unionBySet: Set[String] =
      unionBy(Set("a", "b", "c", "d", "E"), Set("A", "B", "C"))(
        a => a.toLowerCase,
        b => b.toLowerCase
      )
    val actual = areEqual(unionBySet, Set("a", "b", "c", "d", "e"))
    assertEquals(actual, true)
  }

  test("differenceBy") {
    val differenceBySet: Set[String] =
      differenceBy(Set("a", "b", "c", "d", "E"), Set("A", "B", "C"))(
        a => a.toLowerCase,
        b => b.toLowerCase
      )
    val actual = areEqual(differenceBySet, Set("d", "e"))
    assertEquals(actual, true)
  }
}
