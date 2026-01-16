function battery_low_xpm.Image
   return Gtk_Image is
   Pic    : Gdk_Pixbuf := Get_Pixbuf;
   Result : Gtk_Image;
begin
   Gtk_New (Result, Pic);
   Unref (Pic);
   return Result;
end battery_low_xpm.Image;
