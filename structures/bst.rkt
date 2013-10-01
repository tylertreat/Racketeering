; This is an implementation of a binary search tree.

; Construct an empty BST
(define (empty-tree)
  '())

; Check if the given BST is empty
(define (empty-tree? tree)
    (empty? tree))

; Construct a BST with the given root value
(define (bstree root)
  (list root (empty-tree) (empty-tree)))

; Construct a BST with the given root value and children
(define (bstree-with-children root left right)
  (list root left right))

; Return the root value of the BST
(define (tree->root tree)
  (cond
    ((empty-tree? tree) (empty-tree))
    (else (car tree))))

; Return the left subtree of the BST
(define (tree->left tree)
  (cond
    ((empty-tree? tree) (empty-tree))
    (else (list-ref tree 1))))

; Return the right subtree of the BST
(define (tree->right tree)
  (cond
    ((empty-tree? tree) (empty-tree))
    (else (list-ref tree 2))))

; Insert a node with the given value into the BST
(define (insert-tree n t)
  (cond
    ((empty-tree? t) (bstree n))
    ((< n (tree->root t))
     (bstree-with-children (tree->root t)
                           (insert-tree n (tree->left t))
                           (tree->right t)))
    ((> n (tree->root t))
     (bstree-with-children (tree->root t)
                           (tree->left t)
                           (insert-tree n (tree->right t))))
    (else t)))

; Perform an in-order traversal of the BST and return a list of its values
(define (tree->list t)
  (cond
    ((empty-tree? t) '())
    (else (append
           (tree->list (tree->left t))
           (list (tree->root t))
           (tree->list (tree->right t))))))
    
