;; find all buffers visiting files and dirs
;;
;; make a tree from dir to dir to buffer
;; (dir "dir a" "file" (dir "dir b" "file" "file"))
;;
;; data Node = DirNode String [Node] | FileNode String
;;
;; use buffer-file-name to get the file name
;;
;; use file-name-directory to progressively walk up the tree to obtain a
;; list of path components. at each point, use
;;
;; (file-name-nondirectory (directory-file-name "~/bla/blag/"))
;; (file-name-nondirectory (directory-file-name "~/bla/blag"))
;;
;; insert the file/dir-path into the tree
;;
;; sort the tree
;;
;; write the tree into an buffer (with buttons button.el insert-button)
;;
;; bind n & p to next/prev file
;;
;; ? how to distinguish file/dir lines?
