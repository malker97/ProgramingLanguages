#include <iostream>

using namespace std;

int x = 2;
int f(int y){
    return x + y;
}
int main(){
    x = 7;
    cout << x + f(x) << endl;
    // why the result is 21?
    // cuz x is a global variable, so it's under dynamic scoping, so the value is 21
    // if its under static scoping, the value is 14
    return 0;
}