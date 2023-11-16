package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 then 1
    else if r == c then 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def loop(chars: List[Char], acc: Int): Boolean =
      if acc<0 then false
      else if chars.isEmpty & acc!=0 then false
      else if chars.isEmpty & acc==0 then true
      else if chars.head == '(' then loop(chars.tail, acc+1)
      else if chars.head == ')' then loop(chars.tail, acc-1)
      else loop(chars.tail, acc)
    loop(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty then 0
    else if money==0 then 1
    else if money<0 then 0
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)

    /**if coins.isEmpty then 0
    else if money==0%coins.head == 0 then 1
    else if coins.head + coins.tail
    else countChange(money, coins.tail) + countChange(money, coins.head)
    */
}
