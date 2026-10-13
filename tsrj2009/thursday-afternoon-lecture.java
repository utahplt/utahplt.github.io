interface IAnimal {
  boolean isHappyEnough();
  IAnimal scare();
}

class Cat implements IAnimal {
  int x;
  double happiness;
  
  Cat(int x, double happiness) {
    this.x = x;
    this.happiness = happiness;
  }
  
  //  ... method(....) { // this
  //    ... this.x ... this.happiness ...
  //  }
  
  boolean isHappyEnough() { // this
    return this.happiness >= 50;
  }
  
  // To move a cat 20 if it's happy enough
  Cat runIfHappy() {
    if (this.isHappyEnough()) {
      return new Cat(this.x + 20, this.happiness);
    } else {
      return this;
    }
  }
  
  IAnimal scare() {
    return new Cat(this.x, this.happiness - 25.0);
  }
}

class Cham implements IAnimal {
  String color;
  double happiness;
  
  Cham(String color, double happiness) {
    this.color = color;
    this.happiness = happiness;
  }
  
  // ... method(....) {
  //   ... this.color ... this.happiness ...;
  // }
  
  boolean isHappyEnough() {
    return this.happiness >= 50.0;
  }

  IAnimal scare() {
    return new Cham("orange", this.happiness);
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
  
  boolean b1 = check this.joeSr.isHappyEnough() expect true;
  boolean b2 = check new Cat(0, 49.9).isHappyEnough() expect false;
  
  boolean b3 = check this.joeSr.runIfHappy() expect new Cat(30, 50.0);
  boolean b4 = check new Cat(0, 49.9).runIfHappy() expect new Cat(0, 49.9);
  
  boolean b5 = check this.jenny.isHappyEnough() expect true;
  
  boolean b6 = check this.jenny.scare() expect new Cham("orange", 70.0);
  boolean b7 = check this.joeSr.scare() expect new Cat(10, 25.0);
}

// ----------------------------------------

interface IRoom {
  // To count the number of pits reachable from here
  int countPits();
}

class Pit implements IRoom {
  Pit() {}
  
  // ... method(...) { ... }
  
  int countPits() {
    return 1;
  }  
}

class Gold implements IRoom {
  int wt;
  Gold(int wt) {
    this.wt = wt;
  }

  // ... method(...) { ... this.wt ... }

  int countPits() {
    return 0;
  }  
}

class Token implements IRoom {
  int token;
  IRoom left;
  IRoom right;
  Token(int token, IRoom left, IRoom right) {
    this.token = token;
    this.left = left;
    this.right = right;
  }

  // ... method(...) { 
  //   ... this.token 
  //   ... this.left.method(...) 
  //   ... this.right.method(...) ...}

  int countPits() {
    return this.left.countPits() + this.right.countPits();
  }  
}

class MazeExamples {
  MazeExamples( ) { }
  
  IRoom pit1 = new Pit();  
  IRoom gold5 = new Gold(5);
  IRoom pit2 = new Pit();
  IRoom token3 = new Token(3, this.gold5, this.pit2);
  IRoom token1 = new Token(1, this.token3, this.pit1);

  boolean b1 = check this.pit1.countPits() expect 1;
  boolean b2 = check this.gold5.countPits() expect 0;
  boolean b4 = check this.token1.countPits() expect 2;
}
