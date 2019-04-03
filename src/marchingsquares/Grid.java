package marchingsquares;

/**
 * <p>Given a two-dimensional scalar field (rectangular array of individual
 * numerical values) the Marching Squares algorithm applies a <em>threshold</em>
 * (a.k.a contour level or isovalue) to make a binary image containing:</p>
 *
 * <ul>
 * <li>1 where the data value is above the isovalue,</li>
 * <li>0 where the data value is below the isovalue.</li>
 * </ul>
 *
 * <p>Every 2x2 block of pixels in the binary image forms a contouring cell, so
 * the whole image is represented by a grid of such cells (shown in green in
 * the picture below). Note that this contouring grid is one cell smaller in
 * each direction than the original 2D data field.</p>
 */
class Grid
{
   final Cell[][] cells;
   final int rowCount;
   final int colCount;
   final double threshold;
   private transient String str;


   Grid(final Cell[][] cells, final double threshold) {
      super();
      this.cells = cells;
      rowCount = cells.length;
      colCount = cells[0].length;
      this.threshold = threshold;
   }

   Cell getCellAt(final int r, final int c) {
      return cells[r][c];
   }

   int getCellNdxAt(final int r, final int c) {
      final Cell cell = cells[r][c];
      if (cell == null) return 0;
      return cells[r][c].getCellNdx();
   }

   @Override
   public String toString() {
      if (str == null) {
         str = new StringBuilder("Grid{rowCount=").append(rowCount)
               .append(", colCount=").append(colCount)
               .append(", threshold=").append(threshold)
               .append('}')
               .toString();
      }
      return str;
   }

}
