package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: empty string") {
    assert(wordOccurrences("") === List())
  }

  test("wordOccurrences: upper case, single occurrence") {
    assert(wordOccurrences("aBacad") === List(('a', 3), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: mixed case") {
    assert(wordOccurrences("dcbacbcdDd") === List(('a', 1), ('b', 2), ('c', 3), ('d', 4)))
  }

  test("wordOccurrences: reverse") {
    assert(wordOccurrences("dcba") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: Tate") {
    assert(dictionaryByOccurrences(List(('a', 1), ('e', 1), ('t', 2))) == List("Tate"));
  }

  test("dictionaryByOccurrences: smile") {
    assert(dictionaryByOccurrences(List(('e', 1), ('i', 1), ('l', 1), ('m', 1), ('s', 1))) == List("smile", "slime", "Miles", "limes"))
  }

  test("wordAnagrams:smile") {
    assert(wordAnagrams("smile") == List("smile", "slime", "Miles", "limes"))
  }

  test("wordAnagrams: eat") {
    assert(wordAnagrams("eat") == List("tea", "eat", "ate"))
  }

  test("wordAnagrams: spot") {
    assert(wordAnagrams("spot") == List("tops", "stop", "spot", "pots", "post", "opts"))
  }

  test("combinations") {
    assert(combinations(List(('a', 3), ('b', 1), ('c', 2))) == 
      List(List(), List(('c', 1)), List(('c', 2)), List(('b', 1)), 
           List(('b', 1), ('c', 1)), List(('b', 1), ('c', 2)), List(('a', 1)),
           List(('a', 1), ('c', 1)), List(('a', 1), ('c', 2)), List(('a', 1), ('b', 1)), 
           List(('a', 1), ('b', 1), ('c', 1)), List(('a', 1), ('b', 1), ('c', 2)), 
           List(('a', 2)), List(('a', 2), ('c', 1)), List(('a',2), ('c', 2)), 
           List(('a', 2), ('b', 1)), List(('a', 2), ('b', 1), ('c', 1)), 
           List(('a', 2), ('b', 1), ('c', 2)), List(('a', 3)), List(('a', 3), ('c', 1)), 
           List(('a', 3), ('c', 2)), List(('a', 3), ('b', 1)), 
           List(('a', 3), ('b', 1), ('c', 1)), List(('a', 3), ('b', 1), ('c', 2))))
  }

  test("subtract singletons") {
    assert(subtract(List(('a', 3)), List(('a', 2))) == List(('a', 1)))
  }

  test("subtract doubleton") {
    assert(subtract(List(('a', 3), ('b', 2)), List(('b', 1))) == List(('a', 3), ('b', 1)))
  }

  test("subtract doubletons") {
    assert(subtract(List(('a', 3), ('b', 2)), List(('b', 2))) == List(('a', 3)))
  }

  test("subtract identical") {
    assert(subtract(List(('e', 5), ('i', 3), ('l', 4), ('m', 2), ('s', 1)), 
                    List(('e', 5), ('i', 3), ('l', 4), ('m', 2), ('s', 1))) == 
               List())
  }

  test("subtract") {
    assert(subtract(List(('e', 5), ('i', 3), ('l', 4), ('m', 2), ('s', 1)), List(('e', 3), ('i', 3), ('m', 1), ('s', 1))) == 
      List(('e', 2), ('l', 4), ('m', 1)))
  }

  test("sentenceAnagrams") {
    assert(sentenceAnagrams(List("Yes", "man")) == 
      List(List("my", "en", "as"), List("my", "as", "en"), List("my", "Sean"), List("my", "sane"), 
           List("yes", "man"), List("en", "my", "as"), List("en", "as", "my"), List("men", "say"), 
           List("as", "my", "en"), List("as", "en", "my"), List("say", "men"), List("man", "yes"), 
           List("Sean", "my"), List("sane", "my")));
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
