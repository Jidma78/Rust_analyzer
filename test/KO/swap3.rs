fn swap(x : &i32, y : &i32) {
    let t = *x;
    *x = *y;
    *y = t;
}

fn main() {
    let mut x = 1;
    let mut y = 2;
    swap(&mut x, &mut y);
}
