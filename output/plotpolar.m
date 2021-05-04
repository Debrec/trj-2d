xx=[0:5:355];
yy=[-85:5:0];
[X,Y] = meshgrid(xx,yy);
file = ["trjmd" num2str(1) ".dat"];
dat1 = load('-ascii',file);
#z=dat1(:,4);

for i=1:14
  archivo = ["trjmd" num2str(i) ".dat"];
  dat = load('-ascii',archivo);
  for j=1:1224
    londat(i,j) = dat(j,1);
    latdat(i,j) = dat(j,2);
    x(j)=dat(j,1);
    y(j)=dat(j,2);
  end
    clf();
    theta = pi.*londat(i,:)./180;
    rho = (90+latdat(i,:))./90;
    plot(rho.*cos(theta),rho.*sin(theta),'.');
    title(["Figura " num2str(i)]);
    fig = gcf();
    print(fig,'-dpng',["FiguraPolar" num2str(i) ".png"]);
    clf();
    #xyz = [x';y';z']';
    #size(xyz);
	z=dat(:,4);
    Z = griddata(x,y,z,X,Y);
    #fig.color_map=hsvcolormap(32); 
    #colorbar(min(z),max(z));
    mesh(X,Y,Z);
    print(fig,'-dpng',["FiguraNivel" num2str(i) ".png"]);
end
