%  refdifplot.m
%
%  Script file to read in wave height, wave angle, water depth and surface data 
%  from refdif1 output,
%  and construct various plots.  This
%  program uses the quiver routine from Matlab 4.2
%
%  James T. Kirby
%  Center for Applied Coastal Research
%  University of Delaware
%  Newark, DE 19716
%  kirby@coastal.udel.edu, (302) 831-2438, FAX (302) 831-1228.
%
%  11/27/94

%  Read data files.

    load height.dat
    load angle.dat
    load depth.dat

    dx=input(' enter dx:  ');
    dy=input(' enter dy:  ');

    sz=size(height);

%  Compute x,y vectors.

    x=dx*(1:sz(1))-dx;
    y=dy*(1:sz(2))-dy;

%  Constructing scales arrow plot for wave heights and directions.  
%  Compute x,y components of arrows.

    DX=height.*cos(pi*angle/180);
    DY=height.*sin(pi*angle/180);

%  Now do contours of wave height over depth contours.

    figure(1),clf,hold off
    cf=contour(x,y,height');clabel(cf,'manual'),xlabel('x'),ylabel('y')
    hold on,contour(x,y,depth','--'),axis('equal')

%  Now overlay scaled arrows on contours of wave height.

    figure(2),clf,hold off
    cf=contour(x,y,height');clabel(cf,'manual'),xlabel('x'),ylabel('y')
    hold on,quiver(x,y,DX',DY'),axis('equal')

%  Now plot the surface.

    isurf=input('do you want to plot the surface? 1=yes:  ');

    if isurf ==1, load surf.dat

    figure(3),clf,hold off
    cf=contour(x,y,surf');clabel(cf,'manual'),xlabel('x'),ylabel('y')
    hold on, contour(x,y,depth','--'), axis('equal')

    end 
    





