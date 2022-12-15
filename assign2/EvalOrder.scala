// object Solution{
//     def leftToRight( ) : Unit = {
//       println("left-to-right")
//     }
//     def rightToLeft( ) : Unit = {
//       println("right-to-left")
//     }
//     def helperFunction( l: Unit, j: Unit) : Unit = {
//     }
//     def main(args: Array[String]) {
//         helperFunction(leftToRight(), rightToLeft())
//     }
// }
object Solution{
    var p = 0
    def leftToRight( ) : Unit = {
        if(p == 0){
            p += 1
            println("left-to-right")
        }
        // println("left-to-right")
    }
    def rightToLeft( ) : Unit = {
        if(p == 0){
            p += 1
            println("right-to-left")
        }
    //   println("right-to-left")
    }
    def helperFunction( l: Unit, j: Unit) : Unit = {
    }
    def main(args: Array[String]) {
        helperFunction(leftToRight(), rightToLeft())
    }
}