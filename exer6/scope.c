#include <stdio.h>

int main(){
    int a = 0;
    { 
        int a = 1;
        { 
            int b = a;
            int a = 2;
            int c = a;
                {
                    int c=a+b; 
                    printf("a=%d,b=%d,c=%d\n", a, b, c);
                }
        }
    }
    return 0;
}