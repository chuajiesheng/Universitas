type shape = Lines | Square | Rectangle | Circle;;
type point = (int * int);;
type drawing =
  Lines of (point * point)
| Sqaure of (point * point)
| Rectangle of (point * point)
| Circle of (point * int);;
