fn x (y : i32) {
    let mut z = 42 + y;

    fn f(u : i32, v : f32) -> bool {
        let h = 12 + u;

        let r = v + h as f32;

        fn g(a : i8, b : i8) {
            x((a + b) as i32);
        }

        g(1, 2);
        r == 0.
    }

    if f(y, 2.0) { }
    
    z = z - 3;

}

fn main() {
    x(42);
}
