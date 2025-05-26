object Yacht:

   def score(dice: List[Int], category: String): Int =
      val dg = dice.groupBy(identity)
      category match
         case "yacht"           => if dice.forall(_ == dice.head) then 50 else 0
         case "ones"            => dice.filter(_ == 1).sum
         case "twos"            => dice.filter(_ == 2).sum
         case "threes"          => dice.filter(_ == 3).sum
         case "fours"           => dice.filter(_ == 4).sum
         case "fives"           => dice.filter(_ == 5).sum
         case "sixes"           => dice.filter(_ == 6).sum
         case "full house"      => if dg.values.map(_.size).toSeq.sorted == Seq(2, 3) then dice.sum else 0
         case "four of a kind"  =>
            if dg.exists(_._2.size >= 4) then dg.filter(_._2.size >= 4).map(_._2.take(4).sum).max else 0
         case "little straight" => if dice.sorted == List(1, 2, 3, 4, 5) then 30 else 0
         case "big straight"    => if dice.sorted == List(2, 3, 4, 5, 6) then 30 else 0
         case "choice"          => dice.sum
         case _                 => 0
