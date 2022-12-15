#include <stdio.h>

int main(int argc, char *argv[])
{
    int i = 0, a, b, c;
    i++;
    b = (c = 3 + 2);
    a = (a = 10) + (a -= 2) - (a -= 4);
    printf("a = %d, b = %d, c = %d", a, b, c);
    return 0;
}
