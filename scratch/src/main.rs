use bytes::{BytesMut, BufMut, Buf};

fn main() {
    let mut buf = BytesMut::with_capacity(1024);
    //buf.put(&b"hello world"[..]);
    buf.put_u16(1234);
    buf.put_u16(5678);

    let data = buf.get_u16();

    println!("u16 data: {}", data) ;

    let data = buf.get_u16();
    println!("u16 data: {}", data) ;

}
