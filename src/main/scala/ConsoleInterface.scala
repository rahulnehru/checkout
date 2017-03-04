class ConsoleInterface {

    /*
      ConsoleInterface is used to abstract all console interactions.
      This is a means by which the side effect code is isolated.
     */

    def printMsg(msg: String, args: Any *) =
      if(args.isEmpty ) println(msg) else {
        var paramMsg = msg
        for(i <- 0 until args.length ){
          paramMsg = paramMsg.replace(s"{$i}", args(i).toString)
        }
        println(paramMsg)
      }

    def readInput = scala.io.StdIn.readLine()

}
