
pub struct VM {
    pub data: Vec<usize>,
    pub ptr: usize,

}

pub struct Frame<'a> {
    pub data: &'a mut [usize],
    pub ptr: &'a mut usize
}

impl<'a> VM {

    pub fn run(&'a mut self) {

        let mut f =  Frame {
            data: &mut [],
            ptr: &mut self.ptr
        } ;

        self.data.push(0) ;
        *f.ptr=1 ;
        self.data.push(2) ;
        *f.ptr=2 ;
        self.data.push(3) ;
        *f.ptr=3 ;

        f.data = &mut self.data[1..] ;
        f.data[0] = 100 ;

        let mut frames: Vec<Frame<'a>> = Vec::new() ;
        frames.push(f) ;
        frames[0].data[0] = 200 ;


    }
}

fn main() {


    let mut o = VM {
        data: Vec::new(),
        ptr: 1,
    } ;
    o.run();
    println!("{}:{}", o.ptr, o.data[1]);
}
