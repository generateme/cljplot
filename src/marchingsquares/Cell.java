package marchingsquares;

/**
 * <p>A non-immutable class describing a Marching Squares Contour Cell.</p>
 */
class Cell
{
   static enum Side { LEFT, RIGHT, TOP, BOTTOM, NONE }
   private byte cellNdx;
   private final boolean flipped;
   private final float left, top, right, bottom;


   Cell(int ndx, boolean flipped, float left, float top, float right, float bottom) {
      super();
      this.cellNdx = (byte) ndx;
      if (flipped && ndx != 5 && ndx != 10) {
         System.out.println("Cell: Only saddle cells can be flipped. Will set the "
               + "'flipped' flag to FALSE for this (" + ndx + ") cell's index");
         this.flipped = false;
      } else {
         this.flipped = flipped;
      }
      this.left = left;
      this.top = top;
      this.right = right;
      this.bottom = bottom;
   }

   /**
    * <p>Clear this cell's index.</p>
    *
    * <p>When building up shapes, it is possible to have disjoint regions and
    * holes in them. An easy way to build up a new shape from the cell's index
    * is to build sub-paths for one isoline at a time. As the shape is built
    * up, it is necessary to erase the (single) line afterward so that subsequent
    * searches for isolines will not loop indefinitely.
    */
   void clear() {
      switch (cellNdx) {
      case 0:
      case 5:
      case 10:
      case 15:
         break;
      default:
         cellNdx = 15;
      }
   }

   /** @return this cell's algorithm index. */
   byte getCellNdx() {
      return cellNdx;
   }

   /**
    * @param edge which side crossing is wanted.
    * @return crossing coordinates (already) normalized to [0.0..1.0].
    */
   float[] getXY(Side edge) {
      switch (edge) {
      case BOTTOM: return new float[] { bottom, 0.0F };
      case LEFT: return new float[] { 0.0F, left };
      case RIGHT: return new float[] { 1.0F, right };
      case TOP: return new float[] { top, 1.0F };
      default:
         throw new IllegalStateException("getXY: N/A w/o a non-trivial edge");
      }
   }

   /** @return true if this Cell is a Saddle case. Returns false otherwise. */
   boolean isSaddle() {
      return cellNdx == 5 || cellNdx == 10;
   }

   /** @return true if this Cell is trivial; otherwise returns false. */
   boolean isTrivial() {
      return cellNdx == 0 || cellNdx == 15;
   }

   /** @return whether this cell is flipped or not. */
   boolean isFlipped() {
      return flipped;
   }

   @Override
   public String toString() {
      return new StringBuilder("Cell{index=").append(cellNdx)
            .append(", flipped? ").append(flipped)
            .append(", left=").append(left)
            .append(", top=").append(top)
            .append(", right=").append(right)
            .append(", bottom=").append(bottom)
            .append('}')
            .toString();
   }

}
