import java.util.*;

/**
 * Skip Lists support the following operation in expection O(log n) time w.h.p:
 * 
 * Addition
 * Removal
 * Containment
 * Predecessor
 * Sucessor
 *
 */
public class SkipList {

  private Node root;
  private Random rng = new Random(47);
  private int depth;
  
  public SkipList() {
	root = new LeftBoundaryNode();
	root.down = new LeftBoundaryNode();
	root.right = new RightBoundaryNode();
	root.right.down = new RightBoundaryNode();
	root.down.up = root;
	root.right.down.up = root.right;
	root.right.left = root;
	root.down.right = root.right.down;
	root.right.down.left = root.down;
	depth = 1;
  }

  public String toString() {
    return root.toString();
  }

  // ------------------------------------------------------

  /**
   * Inner class representing a non-boundary Node of the skip list
   */
  private class Node {

    public int value;
    public Node left, right, up, down;

    public Node() {
      left = right = up = down = null;
      value = 0;
    }	
    public Node(int i) {
      left = right = up = down = null;
      value = i;
    }

    public boolean isLeftBoundary() {
      return false;
    }

    public boolean isRightBoundary() {
      return false;
    }

 
    public boolean blocked(Node next, int i) {
	if(next.isRightBoundary())
		return true;
        else {
            if(i < next.value)
		return true;
            else
		return false;
        }
    }

   /**
     * Find the node with the largest value not exceeding i
     */
    public Node find(int i) {
        Node current = this; 
	while(true) {
		if(blocked(current.right, i)) {
			if(current.down == null)
				return current;
			else
				current = current.down;
		}
		else {
			current = current.right;
		}
	}

    }

    /**
     * A simple string representation of the node.  Null values don't appear
     */
    public String toString() {
      String s = "[" + this.value + " ";
      if (up != null) {
	s = s + "U: " + up.value + " ";
      } 
      if (down != null) {
	s = s + "D: " + down.value + " ";
      }
      if (left != null) {
	s = s + "L: " + left.value + " ";		
      }
      if (right != null) {
	s = s + "R: " + right.value + " ";	
      }
      s = s + "]";
      return s;
    }
  }

  /**
   * A node representing a left boundary
   */
  private class LeftBoundaryNode extends Node {
    
    public boolean isLeftBoundary() {
      return true;
    }

    public String toString() {
      String s = super.toString() + " -- ";
      Node n = this.right;
      while (!n.isRightBoundary()) {
	s = s + n.toString() + " -- ";
	n = n.right;
      }
      s = s + n.toString() + "\n";
      if (this.down != null) {
	s = s + this.down.toString();
      }
      return s;
    }
  }

  /**
   * A node representing a right boundary
   */
  private class RightBoundaryNode extends Node {
    
    public boolean isRightBoundary() {
      return true;
    }

  }

  public int add(int i) {
	Node target = root.find(i);
	int level = 1;
	Node next = target.right;
	Node me = new Node(i);
	target.right=me; me.left = target;
	me.right = next; next.left = me;
	while(true) {
		if(rng.nextBoolean() && level <= depth) {
			if(this.depth == level) {
				root.up = new LeftBoundaryNode();
				root.up.down = root;
				root.right.up = new RightBoundaryNode();
				root.right.up.down = root.right;
				root.up.right = root.right.up;
				root.right.up.left = root.up;
				root = root.up;
                        }
			me.up = new Node(i);
			me.up.down = me;
			Node cur = me;
			while(cur.right.up == null && !cur.isRightBoundary()) 
				cur = cur.right;
			cur.right.up.left.right = me.up;
			me.up.left = cur.right.up.left;
			me.up.right = cur.right.up;
			cur.right.up.left = me.up;
			level++;
		}
		else {
			if(level > this.depth)
				this.depth = level;
			return level; // the level the node has been added. 
                }
	}
	
  }

  public boolean remove(int i) {
	Node target = root.find(i);
	if(target.isLeftBoundary() || target.isRightBoundary())
		return false;
        else {
		while(target.up != null)
			target = target.up;
		
		while(target != null) {
			target.left.right = target.right;
			target.right.left = target.left;
			if (target.left.isLeftBoundary() && target.right.isRightBoundary() && depth > 1) {
				root=root.down;
				root.up = null;
				root.right.up = null;
				this.depth = depth - 1;
			}
			target = target.down;
		}
		return true;
	}
  }

  public int predecessor(int i) {
	Node target = root.find(i);
	if(target.isLeftBoundary()) {
		return Integer.MAX_VALUE;
	}
	return target.value;	
  }

  public int successor(int i) {
  	Node target = root.find(i);
	if(target.right.isRightBoundary())
		return Integer.MIN_VALUE;
	else
		return target.right.value;
  }

  public int size() {
	int i = 0; 
	Node cur = root;
	while(cur.down != null)
		cur = cur.down;
    	while(!cur.isRightBoundary()) {
		i++;
		cur = cur.right;
	}
	return (i-1);
  }
		

  public static void main(String[] args) {
        SkipList l = new SkipList();
        System.out.println(l);
        System.out.println(l.add(-1000));
        System.out.println(l);
        System.out.println(l.add(3000));
	System.out.println(l.predecessor(0));
	System.out.println(l.successor(0));
        System.out.println(l);
	System.out.println(l.add(57));
	System.out.println(l.add(96));
	System.out.println(l);
	System.out.println(l.remove(-1000));
	System.out.println(l);
	System.out.println(l.remove(3000));
	System.out.println(l);
	l.remove(57);l.remove(96);
	System.out.println(l);
	TreeSet<Integer> set = new TreeSet<Integer>();
	Vector<Integer> list = new Vector<Integer>();
	Vector<Integer> list2 = new Vector<Integer>();
	for(int i=0; i < 50000; i++) {
		list.add(new Integer(l.rng.nextInt()));
	}

	for(int i=0; i < 50000; i++) {
		list2.add(new Integer(l.rng.nextInt()));
	}
//INSERT
	long start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		l.add(list.elementAt(i));
	}
	long end = System.currentTimeMillis();
	System.out.println("SkipList: " + (end - start));
	start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		set.add(list.elementAt(i));
	}	
	end = System.currentTimeMillis();
	System.out.println("TREE: " + (end - start));
	System.out.println(l.depth);
	System.out.println(l.size());
// MORE INSERT
	start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		l.add(list2.elementAt(i));
	}
	end = System.currentTimeMillis();
	System.out.println("SkipList: " + (end - start));
	start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		set.add(list2.elementAt(i));
	}
	end = System.currentTimeMillis();
	System.out.println("TREE: " + (end - start));
	System.out.println(l.depth);
	System.out.println(l.size());
//NOW REMOVE
	start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		l.remove(list.elementAt(i));
	}
	end = System.currentTimeMillis();
	System.out.println("SkipList: " + (end - start));
	start = System.currentTimeMillis();
	for(int i=0; i < 50000; i++) {
		set.remove(list.elementAt(i));
	}	
	end = System.currentTimeMillis();
	System.out.println("TREE: " + (end - start));
	System.out.println(l.depth);
	System.out.println(l.size());
  }
}
