dfecha(1)='01/01/2008 12:00:00';
dfecha(2)='01/01/2008 16:44:59';
dfecha(3)='01/01/2008 21:45:00';
dfecha(4)='02/01/2008 03:00:00';
dfecha(5)='02/01/2008 08:00:00';
dfecha(6)='02/01/2008 13:15:00';
dfecha(7)='02/01/2008 18:14:59';
dfecha(8)='02/01/2008 23:15:00';
dfecha(9)='03/01/2008 04:30:00';
dfecha(10)='03/01/2008 09:30:00';
dfecha(11)='03/01/2008 14:45:00';
dfecha(12)='03/01/2008 19:44:59';
dfecha(13)='04/01/2008 00:45:00';
dfecha(14)='04/01/2008 06:00:00';
dfecha(15)='04/01/2008 11:00:00';
dfecha(16)='04/01/2008 16:15:00';
dfecha(17)='04/01/2008 21:14:59';
dfecha(18)='05/01/2008 02:15:00';
dfecha(19)='05/01/2008 07:30:00';
dfecha(20)='05/01/2008 12:30:00';
dfecha(21)='05/01/2008 17:45:00';
for i=1:21
  archivo = 'trjmd' + string(i) + '.dat';
  dat = read(archivo,-1,6);
  for j=1:1224
    londat(i,j) = dat(j,1);
    latdat(i,j) = dat(j,2);
  end
    clf();
    theta = %pi.*londat(i,:)./180;
    rho = (90+latdat(i,:))./90;
    rho1 = (90+latdat(1,:))./90;
    r=1;
    g=1;
    b=1;
    c=1;
    m=1;
    for k=1:1224
        if (rho1(k) < 0.2) then
            rhor(r)=rho(k);
            thetar(r)=theta(k);
            r=r+1;
        elseif (rho1(k)<0.4) then
            rhog(g)=rho(k);
            thetag(g)=theta(k);
            g=g+1;
        elseif (rho1(k)<0.6) then
            rhob(b)=rho(k);
            thetab(b)=theta(k);
            b=b+1;
        elseif (rho1(k)<0.8) then
            rhoc(c)=rho(k);
            thetac(c)=theta(k);
            c=c+1; 
        elseif (rho1(k)<1.0) then
            rhom(m)=rho(k);
            thetam(m)=theta(k);
            m=m+1; 
        end
    end
    plot(rhor.*cos(thetar),rhor.*sin(thetar),'r.',rhog.*cos(thetag),rhog.*sin(thetag),'g.',rhob.*cos(thetab),rhob.*sin(thetab),'b.',rhoc.*cos(thetac),rhoc.*sin(thetac),'c.',rhom.*cos(thetam),rhom.*sin(thetam),'m.');
    title('Fecha : '+dfecha(i));
    fig = gcf();
    xs2png(fig,'Figura'+string(i)+'.png');
end
