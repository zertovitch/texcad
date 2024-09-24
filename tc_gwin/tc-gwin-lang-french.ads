package TC.GWin.Lang.French is

  m : constant Message_set :=

        (empty      => U (""),
         ready      => U ("Pr�t"),
         ffile      => U ("&Fichier"),
         fnew       => U ("&Nouveau"),
         fopen      => U ("&Ouvrir..."),
         fsave      => U ("&Sauvegarder"),
         fsaveas    => U ("S&auvegarder sous..."),
         fclose     => U ("Fermer"),
         fopen_containing_folder =>
                       U ("Ouvrir &dossier contenant"),
         fexit      => U ("S&ortir"),
         ddraw      => U ("&Dessin"),
         dtext      => U ("&Texte"),
         dframebox  => U ("&Rectangle"),
         dfilledbox => U ("Rectangle &plein"),
         dline      => U ("&Ligne droite  (\line,\vector)"),
         dcircle    => U ("&Cercle"),
         dfcircle   => U ("&Disque"),
         doval      => U ("&Ovale"),
         dbezchain  => U ("Courbes de &B�zier"),
         lline      => U ("&Ligne"),
         lthin      => U ("Lignes &fines (normales)"),
         lthick     => U ("Lignes �p&aisses"),
         lplain     => U ("Lignes p&leines"),
         ldot       => U ("Lignes &pointill�es"),
         ldotp      => U ("Pointill�s: para&m�tres"),
         ldash      => U ("Lignes t&raitill�es"),
         ldashp     => U ("Traitill�s: param�tr&es"),
         lnone      => U ("&Sans fl�che"),
         lhead      => U ("Fl�che en &t�te (e.g. \vector)"),
         lboth      => U ("Fl�che aux &deux bouts"),
         lmiddle    => U ("Fl�che au &milieu"),
         echgtxt    => U ("&Changer texte ou param�tres"),
         epickobj   => U ("E&pingler des objets"),
         epickall   => U ("Epingler tous les objets"),
         eunpickall => U ("Tout lib�rer"),
         etranslate => U ("&Translation"),
         emirror    => U ("S&ym�trie"),
         erotate    => U ("&Rotation"),
         emorphing  => U ("Transformation &affine"),
         edelete    => U ("&Effacer"),
         ecopyclip  => U ("&Copier dans le presse-papier"),
         ecutclip   => U ("&Couper dans le presse-papier"),
         epasteclip => U ("&Coller du presse-papier"),
         eundo      => U ("Annuler"),
         eredo      => U ("R�p�ter"),
         esavemac   => U ("&Ecrire macro"),
         eloadmac   => U ("&Lire macro"),
         eedit      => U ("&Edition"),
         ttools     => U ("&Outils"),
         tclean     => U ("&Nettoyage"),
         vview      => U ("&Affichage"),
         vtogdtb    => U ("Outils de &Dessin"),
         vtogltb    => U ("Param�tres des &lignes"),
         wcascade   => U ("&Cascade"),
         wtilehor   => U ("Classer &horizontalement"),
         wtilever   => U ("Classer &verticalement"),
         wclosall   => U ("Tout fermer"),
         wwindow    => U ("Fe&n�tres"),
         oopt       => U ("&Options"),
         opicopt    => U ("Options du dessin"),
         onewpicopt => U ("Options pour un nouveau dessin"),
         ogenopt    => U ("Options g�n�rales"),
         hhelp      => U ("&Aide"),
         habout     => U ("A &propos de TeXCAD"),
         exists     => U (" existe d�j�."),
         replace    => U ("Voulez-vous le remplacer ?"),
         cannotsave => U ("Impossible d'�crire le fichier (lecture seule ?)"),
         cannotbackup =>
                       U ("Impossible de remplacer la copie de sauvegarde (lecture seule ?)"),
         open       => U ("Ouvrir..."),
         mcancel    => U ("Annuler"),
         save       => U ("Sauvegarder"),
         save_as    => U ("Sauvegarder sous"),
         save_macro => U ("Sauvegarder macro"),
         preview    => U ("Pr�visualiser"),
         prev_fail  => U ("Erreur - LaTeX et DVIWin ou YAP sont-ils install�s ?"),
         close_not_saved =>
                       U ("Fermer dessin"),
         do_you_want_to_save     =>
                       U ("Voulez-vous sauvegarder"),
         the_changes_you_made_to =>
                       U ("les changements apport�s �"),
         lng_chg    => U ("Changement de langue"),
         fx_restrt  => U ("Le changement ne prend effet qu'au red�marrage de TeXCAD."),
         no_picked  => U ("Aucun objet n'est �pingl� !"),
         gen_opt_tab_display       => U ("Affichage"),
         gen_opt_tab_latex         => U ("LaTeX"),
         gen_opt_tab_miscellaneous => U ("Divers"),
         lng        => U ("Langue"),
         gcolors    => U ("Couleurs"),
         background => U ("Arri�re-plan"),
         normal     => U ("Normal"),
         picked     => U ("Epingl�"),
         shadow     => U ("Ombres (structures)"),
         grid       => U ("Grille"),
         gridnone   => U ("Aucune"),
         gridpts    => U ("Points"),
         gridlin    => U ("Lignes"),
         bezpts     => U ("Nouvelles courbes de B�zier pleines: nombre de points"),
         bezauto    => U ("Auto (\qbezier, LaTeX > 2.09)"),
         bezsugg    => U ("Sugg�r� par TeXCAD"),
         param2d_title     => U ("Courbe plane param�trique"),
         param2d_segments  => U ("Segments (0=auto)"),
         param2d_scale     => U ("�chelle"),
         preview_latex_mode => U ("Mode LaTeX pour la pr�visualisation"),
         preview_directory  => U ("R�pertoire pour la pr�visualisation"),
         mcurrent           => U ("Courant"),
         mtemporary         => U ("Temporaire"),
         pic_opt_tab_drawing       => U ("Dessin"),
         pic_opt_tab_latex         => U ("LaTeX"),
         dimensions  => U ("Dimensions"),
         unitlength  => U ("Unit�"),
         linewidth   => U ("�paisseur de ligne"),
         linechain   => U ("Cha�nage de lignes"),
         reduchain   => U ("R�duire lignes bout � bout"),
         slopetol    => U ("Tol�rance de pente"),
         zoom_fac    => U ("Facteur d'agrandissement"),
         qualcirc    => U ("Qualit� des grands cercles"),
         origin      => U ("Origine"),
         choose      => U ("Choisir..."),
         snapping    => U ("Sauts"),
         activated   => U ("Activ�s"),
         stepping    => U ("Longueur"),
         slopes      => U ("Pentes des nouvelles droites"),
         anyslope    => U ("Toutes"),
         txslopes    => U ("Celles de \line"),
         compat      => U ("Compatibilit� (.sty)"),
         preview_insertions =>
                        U ("Insertions LaTeX pour la pr�visualisation"),
         left        => U ("Align� � gauche"),
         hcenter     => U ("Centr� horizontlement"),
         right       => U ("Align� � droite"),
         top         => U ("Align� en haut"),
         vcenter     => U ("Centr� verticalement"),
         bottom      => U ("Align� en bas"),
         dash_size   => U ("Longueur du traitill�"),
         dot_gap     => U ("Ecartement des pointill�s"),
         dot_symbol  => U ("Symbole des pointill�s"),
         new_pic     => U ("Nouveau dessin LaTeX"),
         ltx_pic     => U ("Dessin LaTeX"),
         ltx_pic_bak => U ("Copie de sauvegarde d'un dessin LaTeX"),
         tcd_mac     => U ("Macro TeXCAD"),
         suffix      => U ("Suffixes - extensions"),
         all_files   => U ("Tous les fichiers"),
         noemlines1  => U ("Emlines n'est pas reconnu."),
         noemlines2  => U ("D�sactiver emlines ?"),
         keeporig    => U ("Garder l'original ?"),
         numiter     => U ("Nombre d'it�rations"),
         error       => U ("Erreur"),
         fnotfound   => U ("Fichier inexistant"),
         expl_pick   => U ("Epingler / lib�rer objet / zone"),
         cleanup     => U ("Nettoyer"),
         cleanup_selected =>
                        U ("Nettoyer sujets s�lectionn�s"),
         topic       => U ("Sujet"),
         occurrences => U ("Occurrences"),
         clean_empty_text         => U ("Texte vide"),
         clean_zero_sized_object  => U ("Objet de taille nulle"),
         clean_unknown_command    => U ("Commande inconnue de TeXCAD"),
         clean_comment            => U ("Commentaire TeX"),
         first_pos  => U ("Premi�re position"),
         blurb      => U ("Logiciel libre, sans garantie"),
         authors    => U ("Auteurs:"),
         original   => U ("TeXCAD original pour DOS (1989-1994)"),
         tc4        => U ("syst�me TeXCAD 4"),
         windoze_version =>
                       U ("version pour MS Windows"),
         thanks     => U ("Remerciements:"),
         gwind      => U ("Plateforme GWindows pour GNAT"),
         mouse_drag => U ("L�chez le bouton au 2nd point"),
         bez_pt2    => U ("Cliquez sur l'extr�mit�"),
         bez_ptc    => U ("Cliquez pour fixer la courbe"),
         m_paste    => U ("Cliquer pour situer (0,0)"));

end TC.GWin.Lang.French;
