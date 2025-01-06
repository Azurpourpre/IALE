type 'a t = 
| Node of 'a * 'a t list
| Leaf of 'a