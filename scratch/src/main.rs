
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


        self.data.push(0) ;
        self.data.push(2) ;
        self.data.push(3) ;
        self.data.push(4) ;
        self.data.push(5) ;
        let mut ptr1: *mut usize = self.data[1..].as_mut_ptr();
        let mut ptr2: *mut usize = self.data[2..].as_mut_ptr();
        unsafe {
            ptr1 = ptr1.add(1) ;
            println!("Value {}", *ptr1) ;
            *ptr1 = 100 ;
            println!("Value {}", self.data[2]) ;


            ptr2 = ptr1.offset(1) ;
            println!("Value {}", *ptr2) ;
            ptr2 = &mut 300 ;
            println!("Value {}", *ptr2) ;
        }


        /*
        unsafe {
            let ptr2: *const u32 = buf.as_ptr().offset(buf.len() as isize);
            while ptr1 < ptr2 {
                println!("Address {:?} | Value {}", ptr1, *ptr1);
                ptr1 = ptr1.offset(1);
            }
        }

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
        */


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
