abstract class FamilyTreeNode {
  abstract int geezerCount(int y);
}


class Empty extends FamilyTreeNode {
  Empty() { }
  int geezerCount(int y) {
    return 0; 
  }
}

class Child extends FamilyTreeNode {
   FamilyTreeNode father;
   FamilyTreeNode mother;
   String name;
   int year;
   String eyeColor;
   Child(FamilyTreeNode father,
         FamilyTreeNode mother,
         String name,
         int year,
         String eyeColor) {
      this.father = father;
      this.mother = mother;
      this.name = name;
      this.year = year;
      this.eyeColor = eyeColor;
   }
   int geezerCount(int y) {
     if (this.year < y)
       return (this.father.geezerCount(y)
              + this.mother.geezerCount(y))
              + 1;
     else
       return this.father.geezerCount(y)
              + this.mother.geezerCount(y);
   }
}
