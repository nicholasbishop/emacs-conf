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
    use super::*;

    #[test]
    fn test_asdf() {
        todo!();
    }
}
"))
