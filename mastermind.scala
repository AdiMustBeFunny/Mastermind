package generalPurpose

import scala.util.Random


object Color extends Enumeration{
type Color = Value
  val BLUE,PINK,YELLOW,ORANGE,RED,GREEN,VIOLET,Coral,None=Value
}

object mastermind extends App{

  def genSecretColors[A](seq:Seq[Serializable])(elementsCount:Int):Seq[Serializable]= elementsCount match {

    case 0 => seq
    case _ =>{
      val rnd = new Random()
     genSecretColors(rnd.nextInt(8)%8 match {
        case 0 => seq :+ Color.PINK
        case 1 => seq :+ Color.BLUE
        case 2 => seq :+ Color.Coral
        case 3 => seq :+ Color.GREEN
        case 4 => seq :+ Color.ORANGE
        case 5 => seq :+ Color.RED
        case 6 => seq :+ Color.VIOLET
        case 7 => seq :+ Color.YELLOW
      })(elementsCount-1)

    }
  }
  def removeAt(seq:Seq[Serializable],index:Int):Seq[Serializable]={
    seq.take(index) ++ seq.drop(index+1)
  }

  def cmp(seq:Seq[Serializable],seq2:Seq[Serializable])(cmp:(Serializable,Serializable)=>Boolean):Seq[Boolean]={
    def cmpHelper(seq:Seq[Serializable],seq2:Seq[Serializable])(cmp:(Serializable,Serializable)=>Boolean)(index:Int,seqToReturn:Seq[Boolean]):Seq[Boolean]={
      if(index==seq.size)seqToReturn
      else cmpHelper(seq,seq2)(cmp)(index+1,if(cmp(seq(index),seq2(index)))seqToReturn:+true else seqToReturn:+false )
    }
    cmpHelper(seq,seq2)(cmp)(0,Seq[Boolean]())
  }

  def checkSimmilarities(seq:Seq[Serializable],seqSecret:Seq[Serializable]):Seq[Char]={
    var blackIntoAccount=cmp(seq,seqSecret)((a,b)=>a==b)  //Seq[Boolean]()
    var whiteIntoAccount=Seq[Boolean]()
    var counter=0

    //for special cases
    var whiteRepetitionCheck = Seq[Serializable]()

    while(counter< seq.size)
      {

        var add=false

        if(seq(counter)!=seqSecret(counter))
        {
        var counter2=0
        while(counter2< seq.size)
        {
          if(seq(counter2)==seqSecret(counter2)&&seq(counter2)==seq(counter))
            {
              add=false
            counter2=seq.size //break :)
            }
          else if(seqSecret(counter2)==seq(counter))
            {
              add=true
            }

          counter2+=1
        }
        }
        else
        add=false

        if(add==true)//check wheather we have inserted this color previously, if yes then we don't do this again
          {
            if(whiteRepetitionCheck.contains(seq(counter)))
              {
                add=false
              }
            else
              {
                whiteRepetitionCheck=whiteRepetitionCheck:+seq(counter)
              }
          }

       whiteIntoAccount = whiteIntoAccount :+ add
        counter+=1
      }
  //println(blackIntoAccount.foldLeft(0)((a,b)=>if(b)a+1 else a))
    var rtrn=Seq[Char]()
    val blackCount=blackIntoAccount.foldLeft(0)((a,b)=>if(b)a+1 else a)
    val whiteCount=whiteIntoAccount.foldLeft(0)((a,b)=>if(b)a+1 else a)

    counter=0
    while (counter<blackCount)
      {
        rtrn = rtrn :+ 'B'
        counter+=1
      }
    counter=0
    while (counter<whiteCount)
    {
      rtrn = rtrn :+ 'W'
      counter+=1
    }
    rtrn
  }

  //var seq = genSecretColors(Seq[Serializable]())(4)
  //for(e <-seq )
  //println(e)

  //println(cmp(Seq(Color.YELLOW,Color.BLUE,Color.BLUE),Seq(Color.YELLOW,Color.VIOLET,Color.BLUE))((a,b)=>a==b))
  //println(checkSimmilarities(Seq(Color.RED,Color.RED,Color.BLUE,Color.BLUE),Seq(Color.Coral,Color.YELLOW,Color.RED,Color.RED)))



  //println(secret)
  //println(playerGuess)
  //println((checkSimmilarities(playerGuess,secret)))

  /*
  * Game
  * -----
  * Pick from 8 colors and map them onto 4 slots
  * When you are done selecting finish your turn
  * Repeat until you have successfully recreated secret combination
  * or 9 turns have passed
  *
  * */
  //println("Secret: "+secret)

  val secret = genSecretColors(Seq[Serializable]())(4)
  val playerGuess = scala.collection.mutable.Seq(Color.None,Color.None,Color.None,Color.None)//genSecretColors(Seq[Serializable]())(4)


  var turn=0
  var game_status=0 // 0 - fail 1 - win
  while(turn<9 && game_status==0)
    {
      //select your color
      var selectionDone=false
      while(!selectionDone)
        {
          println("Your sequence: " + playerGuess )
          println("Press 'e' to edit, 'd' to guess")

          var in = io.StdIn.readChar()
          if(in == 'd')
            {
              if(playerGuess.contains(Color.None))
                {
                  println("You can't have unset color fields")
                  //io.StdIn.wait(1000)
                }
              else
                {
                  selectionDone=true
                }
            }
          else
            {//edit :)

              //val BLUE,PINK,YELLOW,ORANGE,RED,GREEN,VIOLET,Coral,None=Value
              println("Select which slot to change (0-3)")
              var slotToChange = -1
              while (slotToChange < 0 || slotToChange > 3) slotToChange = io.StdIn.readInt()
              println("Choose yoour color")
              println("0 - Blue | 1 - Pink | 2 - Yellow | 3 - Orange")
              println("4 - Red | 5 - Green | 6 - Violet | 7 - Coral")
              var colorToPick = -1
              while (colorToPick<0 || colorToPick>7) colorToPick = io.StdIn.readInt()

              colorToPick match {
                case 0 => playerGuess(slotToChange)=Color.BLUE
                case 1 => playerGuess(slotToChange)=Color.PINK
                case 2 => playerGuess(slotToChange)=Color.YELLOW
                case 3 => playerGuess(slotToChange)=Color.ORANGE
                case 4 => playerGuess(slotToChange)=Color.RED
                case 5 => playerGuess(slotToChange)=Color.GREEN
                case 6 => playerGuess(slotToChange)=Color.VIOLET
                case 7 => playerGuess(slotToChange)=Color.Coral
                                }


            }

        }

      if(checkSimmilarities(playerGuess,secret).foldLeft(0)((a,b)=>if(b=='B')a+1 else a)==secret.size)game_status=1
    println("Hint" + checkSimmilarities(playerGuess,secret))
      turn+=1
    }

  if(game_status==1)
    println("You have won")
  else
    println("You have lost")


}
