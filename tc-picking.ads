package TC.Picking is

  px_dist     : constant:= 4.0;
  px_txt_dist2: constant:= 7.0 ** 2;

  type Pick_operation is
    (pick, unpick,
     pick_area, unpick_area,
     pick_all, unpick_all,
     pick_text);

  procedure PicPic(
    p     : in out Picture;
    op    :        Pick_operation;
    M1,M2 : Point:= (0.0,0.0)
  );

  procedure Del_picked( p: in out Picture );

end TC.Picking;