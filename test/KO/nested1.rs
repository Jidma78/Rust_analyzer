fn a () {
    fn b () {
        fn c () {
        }
    }

    c();
}

fn main() {
    a()
}
