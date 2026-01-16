function image_operating_mode_xpm.Image
   return Gtk_Image is
   Pic : constant Gdk_Pixbuf := Get_Pixbuf;
   Result : Gtk_Image;
begin
   Gtk_New (Result, Pic);
   Unref (Pic);
   return Result;
end image_operating_mode_xpm.Image;
