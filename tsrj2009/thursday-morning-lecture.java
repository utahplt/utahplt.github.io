interface IAnimal {
}

class Cat implements IAnimal {
  int x;
  double happiness;
  
  Cat(int x, double happiness) {
    this.x = x;
    this.happiness = happiness;
  }
}

class Cham implements IAnimal {
  String color;
  double happiness;
  
  Cham(String color, double happiness) {
    this.color = color;
    this.happiness = happiness;
  }
}

interface IVFamily {
}

class Empty implements IVFamily {
  Empty() {  }
}

class Parent implements IVFamily {
  String name;
  String kind;
  String place;
  IVFamily child1;
  IVFamily child2;
  
  Parent(String name, String kind, String place,
         IVFamily child1, IVFamily child2) {
    this.name = name;
    this.kind = kind;
    this.place = place;
    this.child1 = child1;
    this.child2 = child2;
  }
}

class Examples {
  Examples() { }
  Cat joeSr = new Cat(10, 50.0);
  Cat cousinMing = new Cat(0, 100.0);
  Cham betty = new Cham("red", 50.0);
  IAnimal jenny = new Cham("blue", 70.0);
  
  IVFamily sue = new Parent("Sue", "cat", "1", new Empty(), new Empty());
  IVFamily frank = new Parent("Frank", "cat", "2", this.sue, new Empty());
  IVFamily ming = new Parent("Ming", "cham", "1", new Empty(), new Empty());
  IVFamily joe = new Parent("Joe", "cat", "2", this.ming, this.frank);
}
