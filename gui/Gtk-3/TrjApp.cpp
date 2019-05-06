#include <gtkmm.h>
#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <vector>

extern "C" void trj_main_();

class datos {
public:
    int nTime, nOut, nPartX, nPartY;
    float mLon, iLon, mLat, iLat;
    std::string windFile, tracFile;
    int isec, day, mon, year;
    float theta;
    int its;
    int gFile;
    std::string File;
    int err;
    float errf;	

    datos() {
      err = -10;
      errf = -1e5;	
    }

    int & get(std::string campo) {
	if (!campo.compare("Ntime")) {
	   return this->nTime;		
	} else if (!campo.compare("Nout")) {
           return this->nOut;
        } else if (!campo.compare("Npartx")) {
           return this->nPartX;
        } else if (!campo.compare("Nparty")) {
           return this->nPartY;
        } else if (!campo.compare("isec")) {
           return this->isec;
        } else if (!campo.compare("day")) {
           return this->day;
        } else if (!campo.compare("month")) {
           return this->mon;
        } else if (!campo.compare("year")) {
           return this->year;
        } else if (!campo.compare("its")) {
           return this->its;
        } else if (!campo.compare("gf")) {
           return this->gFile;
        } else {
           return err;
        }
        return err;
    }	

    float & getf(std::string campo) {
	if (!campo.compare("mlon")) {
	   return this->mLon;		
	} else if (!campo.compare("ilon")) {
           return this->iLon;
        } else if (!campo.compare("mlat")) {
           return this->mLat;
        } else if (!campo.compare("ilat")) {
           return this->iLat;
        } else if (!campo.compare("theta")) {
           return this->theta;
        } else {
           return errf;
        }
        return errf;
    }	

};

int LeerArchivo(std::string nombreArchivo,datos &data) {
    std::ifstream archivo(nombreArchivo.c_str());
	if (!archivo.fail()) {
    		archivo>>data.nTime>>data.nOut;
    		archivo>>data.nPartX>>data.nPartY;
    		archivo>>data.mLon>>data.iLon>>data.mLat>>data.iLat;
    		char buffer1[500],buffer2[500];
    		archivo>>buffer1>>buffer2;
    		data.windFile.assign(buffer1);
    		data.tracFile.assign(buffer2);
    		archivo>>data.isec>>data.day>>data.mon>>data.year;
    		archivo>>data.theta;
    		archivo>>data.its;
    		archivo>>data.gFile;
    		archivo.close();
	} else {
		return 1;
	}
    return 0;
}

int GuardarArchivo(std::string nombreArchivo,datos &data) {
    std::ofstream archivo(nombreArchivo.c_str());
    archivo<<data.nTime<<" "<<data.nOut<<std::endl;
    archivo<<data.nPartX<<" "<<data.nPartY<<std::endl;
    archivo<<data.mLon<<" "<<data.iLon<<" "<<data.mLat<<" "<<data.iLat<<std::endl;
    archivo<<data.windFile.c_str()<<" "<<data.tracFile.c_str()<<std::endl;
    archivo<<data.isec<<" "<<data.day<<" "<<data.mon<<" "<<data.year<<std::endl;
    archivo<<data.theta<<std::endl;
    archivo<<data.its<<std::endl;
    archivo<<data.gFile<<std::endl;
    return 0;
}

datos data;
Glib::RefPtr<Gtk::Builder> refBuilder;

int Resetear() {
    char buffer[500];
    std::vector<std::string> Nombres={"Ntime","Nout","Npartx","Nparty","isec","day","month","year","its","gf"};
    Gtk::Entry* pEntry = nullptr;
    for (int i=0;i<Nombres.size();i++) {
	sprintf(buffer,"%d",data.get(Nombres[i]));
        pEntry = nullptr;
    	refBuilder->get_widget(Nombres[i].c_str(), pEntry);
    	if(pEntry)
    	{
      	    pEntry->set_text(buffer);
    	}
    }

    std::vector<std::string> Nombresf={"mlon","ilon","mlat","ilat","theta"};
    
    for (int i=0;i<Nombresf.size();i++) {
    	sprintf(buffer,"%.2f",data.getf(Nombresf[i]));
	pEntry = nullptr;
    	refBuilder->get_widget(Nombresf[i].c_str(), pEntry);
    	if(pEntry)
    	{
      	    pEntry->set_text(buffer);
    	}
    }

    pEntry = nullptr;
    refBuilder->get_widget("windfile", pEntry);
    if(pEntry)
    {
      pEntry->set_text(data.windFile.c_str());
    }
    pEntry = nullptr;
    refBuilder->get_widget("tracfile", pEntry);
    if(pEntry)
    {
      pEntry->set_text(data.tracFile.c_str());
    }


    return 0;
}

int changeData() {
    char buffer[500];
    std::vector<std::string> Nombres={"Ntime","Nout","Npartx","Nparty","isec","day","month","year","its","gf"};
    Gtk::Entry* pEntry = nullptr;
    for (int i=0;i<Nombres.size();i++) {
        pEntry = nullptr;
    	refBuilder->get_widget(Nombres[i].c_str(), pEntry);
    	if(pEntry)
    	{
      	    strcpy(buffer,pEntry->get_text().c_str());
	    data.get(Nombres[i]) = atoi(buffer);
    	}
    }

    std::vector<std::string> Nombresf={"mlon","ilon","mlat","ilat","theta"};    
    for (int i=0;i<Nombresf.size();i++) {
	pEntry = nullptr;
    	refBuilder->get_widget(Nombresf[i].c_str(), pEntry);
    	if(pEntry)
    	{ 
      	    strcpy(buffer,pEntry->get_text().c_str());
	    data.getf(Nombresf[i]) = atof(buffer);
    	}
    }
  	
    pEntry = nullptr;
    refBuilder->get_widget("windfile", pEntry);
    if(pEntry)
    {
      data.windFile.assign(pEntry->get_text());
    }
    pEntry = nullptr;
    refBuilder->get_widget("tracfile", pEntry);
    if(pEntry)
    {
      data.tracFile.assign(pEntry->get_text());
    }



    return 0;
}


Gtk::ApplicationWindow* pMw = nullptr;

void salir()
{	
  if (pMw)
	  pMw->hide();
}

void guardar() {
  changeData();
  GuardarArchivo("trj-2d.cfg",data);
}

Gtk::ProgressBar* pBarra = nullptr;
Gtk::Button* pRun = nullptr;
Gtk::Button* pPausar = nullptr;
Gtk::Button* pReasumir = nullptr;

void continuar() {
	pRun->set_label("Running..");
	pReasumir->set_sensitive(false);
	pPausar->set_sensitive(true);
	std::ofstream out("status.dat");
	out<<0<<std::endl;		
	out.close();
}

void pausar() {
	pRun->set_label("Paused..");
	pReasumir->set_sensitive(true);
	pPausar->set_sensitive(false);
	std::ofstream out("status.dat");
	out<<1<<std::endl;		
	out.close();			
}

class Worker {
  public:
 
    Worker() : thread1(0),thread2(0) {
	  std::ofstream out("status.dat");
	  out<<0<<std::endl;
	  out.close();	
	  out.open("pbar.dat");
	  out<<0<<std::endl;
	  out.close();		
	}
 
    // Called to start the processing on the thread
    void start () {
      thread1 = Glib::Thread::create(sigc::mem_fun(*this, &Worker::run), true);
      thread2 = Glib::Thread::create(sigc::mem_fun(*this, &Worker::readpercent), true);
    }
 
    ~Worker() {
      if (thread1)
        thread1->join();

	  if(thread2)
		thread2->join();    
	}
 
    Glib::Dispatcher sig_done;
 
  protected:
	Glib::Thread * thread1;
	Glib::Thread * thread2;

    void run () {
        trj_main_();
        sig_done();
    }

	void readpercent() {
		int percent = 0,percentp=0;
		do {
			std::ifstream in("pbar.dat");
			in>>percent;
			in.close();
			if (percent != percentp) {				
				pBarra->set_fraction(percent/100.);						
				percentp = percent;
				sleep(2);	
			}
		} while (percent<100);
	}
};

class Model {
	public:	
	Worker * w;

	Model () {
		w=NULL;		
	}

	~Model () {
		delete w;
		w = NULL;
	}
	

	void run_model() {
		w = new Worker();
    	w->sig_done.connect(sigc::mem_fun(*this, &Model::Done));
    	w->start();
		pRun->set_label("Running..");
		pRun->set_sensitive(false);
		pReasumir->set_sensitive(false);		
	}

	void Done () {
      delete w;
      w = NULL;
	  pRun->set_label("Stop!!!");
	  pPausar->set_sensitive(false);		
    }
 
};

int main (int argc, char **argv)
{
  	//if(!Glib::thread_supported()) Glib::thread_init();
  
  	auto app = Gtk::Application::create(argc, argv, "trj.cfg");

  	//Load the GtkBuilder file and instantiate its widgets:
  	refBuilder = Gtk::Builder::create();
  	try
 	 {
    	refBuilder->add_from_file("TrjApp.glade");
 	 }
  	catch(const Glib::FileError& ex)
  	{
    	std::cerr << "FileError: " << ex.what() << std::endl;
    	return 1;
  	}
 	 catch(const Glib::MarkupError& ex)
  	{
    	std::cerr << "MarkupError: " << ex.what() << std::endl;
    	return 1;
  	}
  	catch(const Gtk::BuilderError& ex)
  	{
    	std::cerr << "BuilderError: " << ex.what() << std::endl;
    	return 1;
  	}

  	//Get the GtkBuilder-instantiated Window:
  	refBuilder->get_widget("TrjApp", pMw);
  	if(pMw)
  	{
    	LeerArchivo("trj-2d.cfg",data);
    	Resetear();
 
    	Gtk::Button* pSalir = nullptr;
    	refBuilder->get_widget("Salir", pSalir);
    
    	if(pSalir)
   	    {
    	  pSalir->signal_clicked().connect(sigc::ptr_fun(&salir));
    	}
    
    	Gtk::Button* pGuardar = nullptr;
    	refBuilder->get_widget("Guardar", pGuardar);
    
  		if(pGuardar)
   		{
      		pGuardar->signal_clicked().connect(sigc::ptr_fun(&guardar) );
   		}

		refBuilder->get_widget("barraperc",pBarra);

		Model m;

		refBuilder->get_widget("Run",pRun);
		if(pRun) {
			pRun->signal_clicked().connect(sigc::mem_fun(m,&Model::run_model));
		}

	    refBuilder->get_widget("Pausar", pPausar);
    
	    if(pPausar)
	    {
	      pPausar->signal_clicked().connect(sigc::ptr_fun(&pausar));
	    }

	    refBuilder->get_widget("Reasumir", pReasumir);
    
	    if(pReasumir)
	    {
	      pReasumir->signal_clicked().connect(sigc::ptr_fun(&continuar));
	    }
 	
		app->run(*pMw);
	  }

  	  delete pMw;

	return 0;
}
