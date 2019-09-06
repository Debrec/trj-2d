theta = %pi.*londat(1,:)./180;
rho = (90+latdat(1,:))./90;
plot(rho.*cos(theta),rho.*sin(theta),'.');
