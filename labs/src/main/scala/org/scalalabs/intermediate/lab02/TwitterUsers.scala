package org.scalalabs.intermediate.lab02


object TwitterUsers {
  // insert beautiful Scala code here please...

  // TwitterUsers are popular if they have at least 2000 followers
  def thatArePopular(list: List[TwitterUser]): List[TwitterUser] = list.filter( _.followersCount >= 2000)

  def thatArePopularByScreenName(list: List[TwitterUser]): List[String] = thatArePopular(list).filter(_.screen_name !="").map(_.screen_name)

  def thatArePopularByScreenNameSortedbyPopularity(list : List[TwitterUser]): List[String] = thatArePopular(list).sortWith(compareFollowerCount(_, _)).map(_.screen_name)


  def thatArePopularByScreenNameAndPopularitySortedbyPopularity(list: List[TwitterUser]): List[(String, Int)] = {
    for {user <- thatArePopular(list).sortWith(compareFollowerCount(_, _))
    } yield {
      (user.screen_name, user.followersCount)
    }
  }

  def thatAreInBothLists(friends: List[TwitterUser], followers: List[TwitterUser]): List[TwitterUser] =  friends intersect followers


  private def compareFollowerCount(a: TwitterUser, b: TwitterUser): Boolean = {
     a.followersCount - b.followersCount > 0

  }

}

/**
  *  All the methods from before are now called as
  * if they were methods on the List class itself...
  */

object TwitterUsersBonus {
  implicit def convertTwitterUsers(list: List[TwitterUser]) = new TwitterUserBonus(list)
}

class TwitterUserBonus(list: List[TwitterUser]) {
   def thatArePopular = TwitterUsers.thatArePopular(list)
   def thatArePopularByScreenName(): List[String] = TwitterUsers.thatArePopularByScreenName(list)
   def thatArePopularByScreenNameSortedbyPopularity(): List[String] = TwitterUsers.thatArePopularByScreenNameSortedbyPopularity(list)
   def thatArePopularByScreenNameAndPopularitySortedbyPopularity = TwitterUsers.thatArePopularByScreenNameAndPopularitySortedbyPopularity(list)
   def thatAreAlsoIn(that: List[TwitterUser]) = TwitterUsers.thatAreInBothLists(list, that)
}



