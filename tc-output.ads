with Ada.Text_IO;

package TC.Output is

  procedure Save( pic           : in out Picture;
                  macro         :        Boolean;
                  file_name     :        String;
                  displayed_name:        String
  );

  procedure Insert( pic           : in Picture;
                    macro         :    Boolean;
                    file          :    Ada.Text_IO.File_Type;
                    displayed_name:    String
  );
  --  Insert: does not open & close file, just write the picture data
  --  into an open (LaTeX) file.

  procedure Insert_and_Wrap_in_document(
    pic  : in Picture;
    macro:    Boolean;
    file :    Ada.Text_IO.File_Type;
    title:    String
  );

end TC.Output;
