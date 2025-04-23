fn main() {
    let x = 42;
    let y = x;
    let mut z = &y;
    let x = &mut z;
    let y = -y;
    let z = !y;
    let x = *x;
    let b = *x + y - z == *x * y / z && *x % y | z != *x & y ^ z ||
        *x << y <= y >> z && *x < z || y > *x && *x >= 10;
    let y = b as i8;
    let mut z =  2e-20 + (y as i32 - *x) as f32;
    let x = &mut &mut z;
    let _ = **x = 3.14159__________;

    let _ = loop {
        if **x == 12. {
            continue; };
        if **x < 2. {
            break 26;
        };
        **x = 0.;
    };
    
}
