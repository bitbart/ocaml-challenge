# Inorder numbering of binary trees

Consider the following type to represent (labelled) binary trees:
```ocaml
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;
```

An *inorder* traversal of a binary tree first visits recursively the left subtree, then it visits the root and finally it moves on to visit recursively the right subtree.

Write a function with type:
```ocaml
number_tree : 'a btree -> (int * 'a) btree
```
that assigns to each data point in the tree the step it would be visited by an inorder traversal. The nodes of the output tree are pairs where the first component is the step
number and the right component is the original value.

For example, given the binary tree of strings:
```ocaml
let t =
  Node("d",
    Node("c",
      Node("a",
        Empty,
        Node("b",Empty,Empty)),
      Empty),
    Node("e",Empty,Empty))
```
Your function should pass the following test:
```ocaml
assert (number_tree t =
  Node((3,"d"),
    Node((2,"c"),
      Node((0,"a"),
        Empty,
        Node((1,"b"),Empty,Empty)),
    Empty),
  Node((4,"e"),Empty,Empty)))
```
*Hint*: you may need to remember some value between recursive calls...