// Author: Zhengmao Zhang
// int main(){
//     int a = 0;
//     { 
//         int a = 1;
//         { 
//             int b = a;
//             int a = 2;
//             int c = a;
//                 {
//                     int c=a+b; 
//                     printf("a=%d,b=%d,c=%d\n", a, b, c);
//                 }
//         }
//     }
//     return 0;
// }
object Scope {
    def main(args: Array[String]) = {
        var flag = true
        var a = 0
        if(flag){
            var a = 1
            var temp = a
            if(flag){
                var b = temp
                var a = 2
                var c = a
                if(flag){
                    var c = a + b
                    println("a=" + a + ",b=" + b + ",c=" + c)
                }
            }
        }
    }
}