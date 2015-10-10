import org.scalatest._

class UPGMALargeTest extends FlatSpec with Matchers with MapEquality {

  it should "pass the test" in {
    val inp =
    """27
      |0 558 767 179 821 142 317 535 297 834 675 450 808 729 586 792 292 51 101 67 122 900 257 500 569 88 442
      |558 0 119 623 974 855 389 185 548 361 266 636 284 958 635 255 372 640 128 718 64 619 752 846 105 927 995
      |767 119 0 982 176 433 684 903 547 744 585 369 795 309 510 734 594 581 695 845 172 247 391 521 299 220 610
      |179 623 982 0 170 79 302 613 566 494 330 649 210 819 826 413 544 322 951 488 572 705 476 560 192 377 955
      |821 974 176 170 0 909 608 50 954 103 202 52 378 645 357 733 262 412 897 551 181 787 672 611 854 591 699
      |142 855 433 79 909 0 781 491 132 394 972 203 789 782 168 597 643 756 835 797 435 133 520 763 54 910 768
      |317 389 684 302 608 781 0 757 77 353 667 946 209 321 908 90 499 719 151 99 221 419 334 485 741 701 936
      |535 185 903 613 50 491 757 0 350 971 337 131 525 754 235 347 349 264 959 704 549 860 466 364 721 546 356
      |297 548 547 566 954 132 77 350 0 254 314 573 447 376 671 94 858 748 702 303 561 401 824 56 617 218 916
      |834 361 744 494 103 394 353 971 254 0 290 523 420 968 664 116 440 543 931 312 265 990 204 944 938 930 657
      |675 266 585 330 202 972 667 337 314 290 0 755 791 346 899 578 336 234 921 524 539 928 503 315 999 711 467
      |450 636 369 649 52 203 946 131 573 523 755 0 989 189 198 518 737 126 139 287 432 891 324 462 143 227 765
      |808 284 795 210 378 789 209 525 447 420 791 989 0 565 456 790 138 785 874 917 60 289 725 934 121 1000 817
      |729 958 309 819 645 782 321 754 376 968 346 189 565 0 519 245 552 694 81 877 678 842 215 751 300 57 277
      |586 635 510 826 357 168 908 235 671 664 899 198 456 519 0 393 600 911 276 844 894 590 478 71 532 631 550
      |792 255 734 413 733 597 90 347 94 116 578 518 790 245 393 0 697 761 461 490 464 967 459 501 448 832 624
      |292 372 594 544 262 643 499 349 858 440 336 737 138 552 600 697 0 508 316 166 820 871 288 584 130 301 477
      |51 640 581 322 412 756 719 264 748 543 234 126 785 694 911 761 508 0 230 576 919 775 425 801 985 626 814
      |101 128 695 951 897 835 151 959 702 931 921 139 874 81 276 461 316 230 0 282 713 238 137 913 239 682 592
      |67 718 845 488 551 797 99 704 303 312 524 287 917 877 844 490 166 576 282 0 406 91 637 177 969 162 326
      |122 64 172 572 181 435 221 549 561 265 539 432 60 678 894 464 820 919 713 406 0 807 106 522 258 847 123
      |900 619 247 705 787 133 419 860 401 990 928 891 289 842 590 967 871 775 238 91 807 0 964 368 455 320 457
      |257 752 391 476 672 520 334 466 824 204 503 324 725 215 478 459 288 425 137 637 106 964 0 882 451 559 716
      |500 846 521 560 611 763 485 364 56 944 315 462 934 751 71 501 584 801 913 177 522 368 882 0 598 601 184
      |569 105 299 192 854 54 741 721 617 938 999 143 121 300 532 448 130 985 239 969 258 455 451 598 0 207 630
      |88 927 220 377 591 910 701 546 218 930 711 227 1000 57 631 832 301 626 682 162 847 320 559 601 207 0 984
      |442 995 610 955 699 768 936 356 916 657 467 765 817 277 550 624 477 814 592 326 123 457 716 184 630 984 0""".stripMargin

    val resStr =
      """0->28:25.500
        |1->36:59.500
        |2->36:59.500
        |3->37:67.750
        |4->27:25.000
        |5->29:27.000
        |6->33:45.000
        |7->27:25.000
        |8->30:28.000
        |9->39:117.250
        |10->44:168.000
        |11->35:45.750
        |12->32:30.000
        |13->31:28.500
        |14->41:131.667
        |15->33:45.000
        |16->44:168.000
        |17->28:25.500
        |18->38:68.500
        |19->34:45.500
        |20->32:30.000
        |21->34:45.500
        |22->38:68.500
        |23->30:28.000
        |24->29:27.000
        |25->31:28.500
        |26->48:235.375
        |27->4:25.000
        |27->35:20.750
        |27->7:25.000
        |28->17:25.500
        |28->40:101.125
        |28->0:25.500
        |29->24:27.000
        |29->5:27.000
        |29->37:40.750
        |30->8:28.000
        |30->42:128.125
        |30->23:28.000
        |31->25:28.500
        |31->13:28.500
        |31->45:201.125
        |32->12:30.000
        |32->43:134.375
        |32->20:30.000
        |33->39:72.250
        |33->6:45.000
        |33->15:45.000
        |34->42:110.625
        |34->19:45.500
        |34->21:45.500
        |35->41:85.917
        |35->27:20.750
        |35->11:45.750
        |36->1:59.500
        |36->43:104.875
        |36->2:59.500
        |37->3:67.750
        |37->49:189.155
        |37->29:40.750
        |38->40:58.125
        |38->18:68.500
        |38->22:68.500
        |39->9:117.250
        |39->33:72.250
        |39->46:113.417
        |40->38:58.125
        |40->28:101.125
        |40->45:103.000
        |41->35:85.917
        |41->14:131.667
        |41->50:127.068
        |42->48:79.250
        |42->30:128.125
        |42->34:110.625
        |43->32:134.375
        |43->46:66.292
        |43->36:104.875
        |44->47:67.292
        |44->16:168.000
        |44->10:168.000
        |45->31:201.125
        |45->47:5.667
        |45->40:103.000
        |46->39:113.417
        |46->43:66.292
        |46->49:26.238
        |47->44:67.292
        |47->50:23.443
        |47->45:5.667
        |48->26:235.375
        |48->52:59.289
        |48->42:79.250
        |49->51:29.874
        |49->46:26.238
        |49->37:189.155
        |50->41:127.068
        |50->47:23.443
        |50->51:28.045
        |51->52:7.884
        |51->50:28.045
        |51->49:29.874
        |52->48:59.289
        |52->51:7.884""".stripMargin.lines

    val res =
      ( resStr
        .map(_.split("->|:") match {case Array(s1,s2,s3) => ((s1.toInt,s2.toInt) -> s3.toDouble)})
        .toMap )

    mapEq(UPGMA.upgma(inp.lines), res) should be (true)
  }
}