// #include <stdio.h>

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

class Scope2 {
    public static void main(String[] args) {
        int a = 0;
        { 
            int a = 1;
            { 
                int b = a;
                int a = 2;
                int c = a;
                    {
                        int c=a+b; 
                        System.out.println("a="+a+",b="+b+",c="+c);
                    }
            }
        }
    }
}