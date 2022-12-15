// class P {
//          int a;
//            P (int a) { this.a = a; }
//            void print() { System.out.println(a); }
// }
//          class TestParam {
//            static void f(P x, P y, P z) {
//              z = x; x = y; y = z;
//            }
//            static void g(P x, P y, P z) {
//              z.a = x.a; x.a = y.a; y.a = z.a;
//            }
//            public static void main(String argv[]) {
//              P p1 = new P(1), p2 = new P(2), p3 = new P(3);
//              f(p1, p2, p3);
//              p1.print(); p2.print(); p3.print();
//              g(p1, p2, p3);
//              p1.print(); p2.print(); p3.print();
//            }
// }
class P {
    int a;
    P (int a) { this.a = a; }
    void print() { System.out.println(a); }
}
class TestParam {
    static void f(P x, P y, P z) {
        z = x; x = y; y = z;
    }
    static void g(P x, P y, P z) {
        z.a = x.a; x.a = y.a; y.a = z.a;
    }
    public static void main(String argv[]) {
        P p1 = new P(1), p2 = new P(2), p3 = new P(3);
        f(p1, p2, p3);
        p1.print(); p2.print(); p3.print();
        g(p1, p2, p3);
        p1.print(); p2.print(); p3.print();
        g(p1, p2, p3);
        p1.print(); p2.print(); p3.print();
        // 1 2 3
        // 2 1 1
    }
}