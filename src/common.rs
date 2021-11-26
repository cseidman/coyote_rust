pub fn BytesToU64(bytes: &[u8]) -> u64 {
    let mut val:[u8;8] = Default::default();
    val.copy_from_slice(bytes) ;
    u64::from_be_bytes(val)
}

#[cfg(test)]
mod test {

    use super::* ;

    #[test]
    fn test_bytes() {
        // Get a bag of bytes
        let num = 125489 as u64 ;
        let b= &num.to_be_bytes()[0..8] ;
        let val = BytesToU64(b) ;
        assert_eq!(num, val) ;

    }
}