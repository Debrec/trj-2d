xx=[0:5:355];
yy=[-85:5:0];
[X,Y] = meshgrid(xx,yy);
file = 'trjmd' + string(1) + '.dat';
dat1 = read(file,-1,6);
z=dat1(:,2);

for i=1:21
  archivo = 'trjmd' + string(i) + '.dat';
  dat = read(archivo,-1,6);
  for j=1:1224
    londat(i,j) = dat(j,1);
    latdat(i,j) = dat(j,2);
    x(j)=dat(j,1);
    y(j)=dat(j,2);
  end
    clf();
    theta = %pi.*londat(i,:)./180;
    rho = (90+latdat(i,:))./90;
    plot(rho.*cos(theta),rho.*sin(theta),'.');
    title('Figura '+string(i));
    fig = gcf();
    xs2png(fig,'FiguraPolar'+string(i)+'.png');
    clf();
    xyz = [x';y';z']';
    size(xyz);
    Z = eval_cshep2d(X,Y,cshep2d(xyz));
    fig.color_map=hsvcolormap(32); 
    colorbar(min(z),max(z));
    contourf(xx,yy,Z');
    xs2png(fig,'FiguraNivel'+string(i)+'.png');
end
