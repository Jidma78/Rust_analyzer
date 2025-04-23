fn swap(x : &mut i32, y : &mut i32) {
    let t = *x;
    *x = *y;
    *y = t;
}

fn main() {
    let x = 1;
    let y = 2;
    swap(&mut x, &mut y);
}
