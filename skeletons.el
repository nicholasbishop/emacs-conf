(defun skel-py ()
  (interactive)
  (insert "#!/usr/bin/env python3

def main():
    pass


if __name__ == \"__main__\":
    main()
"))

(defun skel-rust-tests()
  (interactive)
  (insert "#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
"))
