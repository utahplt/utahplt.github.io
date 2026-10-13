interface IRoom {
  // To count the number of pits reachable from here
  int countPits();
  // Return the best possible score for this maze given how many tokens we've seen so far
  int bestScoreGT(int tokensSoFar);
}

class Pit implements IRoom {
  Pit() {}
  
  // ... method(...) { ... }
  int bestScoreGT(int tokensSoFar) {
    return 0;
  }
  
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
  int bestScoreGT(int tokensSoFar) {
    return this.wt * tokensSoFar;
  }

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
  int bestScoreGT(int tokensSoFar) {
    return Math.max(this.left.bestScoreGT(this.token + tokensSoFar),
                    this.right.bestScoreGT(this.token + tokensSoFar));
  }

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
  
  boolean b5 = check this.pit1.bestScoreGT(0) expect 0;
  boolean b6a = check this.gold5.bestScoreGT(0) expect 0;
  boolean b6b = check this.gold5.bestScoreGT(1) expect 5;
  boolean b7 = check this.pit2.bestScoreGT(0) expect 0;
  boolean b8 = check this.token3.bestScoreGT(0) expect 15;
  boolean b9 = check this.token1.bestScoreGT(0) expect 20;
}
