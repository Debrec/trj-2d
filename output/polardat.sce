theta = %pi.*londat(18,:)./180;
rho = (90+latdat(18,:))./90;
plot(rho.*cos(theta),rho.*sin(theta),'.');
