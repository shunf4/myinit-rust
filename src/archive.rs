use std::cell::{RefMut, RefCell};
use std::rc::Rc;
use std::io::prelude::*;
use tar::{Archive, Builder};
use std::fs::File;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use std::ops::Deref;
use std::ops::DerefMut;
use std::convert::Into;

pub struct MyInitArchive {
    inner: RefCell<File>,
}

pub struct LiveArchive<'a> {
    inner_file: RefMut<'a, File>,
}

impl<'a, 'b> LiveArchive<'a> {
    pub fn get(&'b mut self) -> Archive<GzDecoder<&'b mut File>> {
        self.inner_file.seek(std::io::SeekFrom::Start(0)).unwrap();

        Archive::new(
            GzDecoder::new(
                &mut *self.inner_file
            )
        )
    }
}

impl<'a, 'b> Into<Archive<GzDecoder<&'b mut File>>> for &'b mut LiveArchive<'a> {
    fn into(self) -> Archive<GzDecoder<&'b mut File>> {
        self.inner_file.seek(std::io::SeekFrom::Start(0)).unwrap();

        Archive::new(
            GzDecoder::new(
                &mut *((*self).inner_file)
            )
        )
    }
}

impl MyInitArchive {
    pub fn new(inner_file: File) -> Self {
        MyInitArchive {
            inner: RefCell::new(inner_file),
        }
    }

    pub fn tar<'a, 'b>(&'a self) -> LiveArchive<'b> where 'a: 'b {
        let file_ref_mut = self.inner.borrow_mut();
        LiveArchive {
            inner_file: file_ref_mut,
        }
    }
}