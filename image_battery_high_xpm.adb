with Gdk.Pixbuf.Conversions;
package body image_battery_high_xpm is
   function Get_Pixbuf return Gdk_Pixbuf is
      function Internal
               (  Data       : Pixbuf_Image;
                  Colorspace : Gdk_Colorspace;
                  Has_Alpha  : GBoolean;
                  Bits       : Int;
                  Width      : Int;
                  Height     : Int;
                  Rowstride  : Int;
                  Fn         : Address;
                  Fn_Data    : Address
               )  return Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_data");
   begin
      return Gdk.Pixbuf.Conversions.From_Address
             (  Internal
                (  Pixels,
                   Colorspace_RGB,
                   1,
                   8,
                   Int (X_Size),
                   Int (Y_Size),
                   64,
                   Null_Address,
                   Null_Address
             )  );
   end Get_Pixbuf;
end image_battery_high_xpm;
