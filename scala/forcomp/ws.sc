import forcomp._
import Anagrams._

object ws {
  wordOccurrences("")                             //> res0: forcomp.Anagrams.Occurrences = List()
  wordOccurrences("abcd")                         //> res1: forcomp.Anagrams.Occurrences = List((a,1), (b,1), (c,1), (d,1))
  wordOccurrences("aBacad")                       //> res2: forcomp.Anagrams.Occurrences = List((a,3), (b,1), (c,1), (d,1))
  wordOccurrences("dcbacbcdDd")                   //> res3: forcomp.Anagrams.Occurrences = List((a,1), (b,2), (c,3), (d,4))
  wordOccurrences("dcba")                         //> res4: forcomp.Anagrams.Occurrences = List((a,1), (b,1), (c,1), (d,1))
  dictionaryByOccurrences(List(('a',1),('e',1),('t',1)))
                                                  //> res5: List[forcomp.Anagrams.Word] = List(tea, eat, ate)
  dictionaryByOccurrences(List(('a',1),('e',1),('t',2)))
                                                  //> res6: List[forcomp.Anagrams.Word] = List(Tate)
  dictionaryByOccurrences(List(('e',1),('i',1),('l',1),('m',1),('s',1)))
                                                  //> res7: List[forcomp.Anagrams.Word] = List(smile, slime, Miles, limes)
  wordAnagrams("smile")                           //> res8: List[forcomp.Anagrams.Word] = List(smile, slime, Miles, limes)
  wordAnagrams("eat")                             //> res9: List[forcomp.Anagrams.Word] = List(tea, eat, ate)
  wordAnagrams("spot")                            //> res10: List[forcomp.Anagrams.Word] = List(tops, stop, spot, pots, post, opts
                                                  //| )
  combinations(List(('a',3),('b',1),('c',2)))     //> res11: List[forcomp.Anagrams.Occurrences] = List(List(), List((c,1)), List((
                                                  //| c,2)), List((b,1)), List((b,1), (c,1)), List((b,1), (c,2)), List((a,1)), Lis
                                                  //| t((a,1), (c,1)), List((a,1), (c,2)), List((a,1), (b,1)), List((a,1), (b,1), 
                                                  //| (c,1)), List((a,1), (b,1), (c,2)), List((a,2)), List((a,2), (c,1)), List((a,
                                                  //| 2), (c,2)), List((a,2), (b,1)), List((a,2), (b,1), (c,1)), List((a,2), (b,1)
                                                  //| , (c,2)), List((a,3)), List((a,3), (c,1)), List((a,3), (c,2)), List((a,3), (
                                                  //| b,1)), List((a,3), (b,1), (c,1)), List((a,3), (b,1), (c,2)))
  subtract(List(('a',3)),List(('a',2)))           //> res12: forcomp.Anagrams.Occurrences = List((a,1))
  subtract(List(('a',3),('b',2)),List(('b',1)))   //> res13: forcomp.Anagrams.Occurrences = List((a,3), (b,1))
  subtract(List(('a',3),('b',2)),List(('b',2)))   //> res14: forcomp.Anagrams.Occurrences = List((a,3))
  subtract(List(('e',5),('i',3),('l',4),('m',2),('s',1)),List(('e',5),('i',3),('l',4),('m',2),('s',1)))
                                                  //> res15: forcomp.Anagrams.Occurrences = List()
  subtract(List(('e',5),('i',3),('l',4),('m',2),('s',1)),List(('e',3),('i',3),('m',1),('s',1)))
                                                  //> res16: forcomp.Anagrams.Occurrences = List((e,2), (l,4), (m,1))
  sentenceAnagrams(List("Yes", "man"))            //> res17: List[forcomp.Anagrams.Sentence] = List(List(my, en, as), List(my, as,
                                                  //|  en), List(my, Sean), List(my, sane), List(yes, man), List(en, my, as), List
                                                  //| (en, as, my), List(men, say), List(as, my, en), List(as, en, my), List(say, 
                                                  //| men), List(man, yes), List(Sean, my), List(sane, my))
                                                
}