fd=mopen('test.txt');
for i=1:2
    [nro,dia,mes,ano,hora,minutos,segundos] = mscanf(fd,"\d \d/\d/\d \d:\d:\d");
end
mclose(fd);
