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
  if(pMw)
    pMw->hide();
}

void guardar() {
  changeData();
  GuardarArchivo("trj-2d.cfg",data);
}

class Worker {
  public:
 
    Worker() : thread(0), stop(false) {}
 
    // Called to start the processing on the thread
    void start () {
      thread = Glib::Thread::create(sigc::mem_fun(*this, &Worker::run), true);
    }
 
    // When shutting down, we need to stop the thread
    ~Worker() {
      {
        Glib::Mutex::Lock lock (mutex);
        stop = true;
      }
      if (thread)
        thread->join(); // Here we block to truly wait for the thread to complete
    }
 
    Glib::Dispatcher sig_done;
 
  protected:
	Glib::Thread * thread;
    Glib::Mutex mutex;
    bool stop;

    // This is where the real work happens
    void run () {
 
      while(true) {
        {
          Glib::Mutex::Lock lock (mutex);
          if (stop) break;
        }
        trj_main_();
        sig_done();
        break;
      }
    }
};

class model {
	public:	
	Worker * w;

	model () {
		w=NULL;		
	}

	void run_model() {
		w = new Worker();
    	w->sig_done.connect(sigc::mem_fun(*this, &model::Done));
    	w->start();
	}

	void Done () {
      delete w;
      w = NULL;
    }
 
};

int main (int argc, char **argv)
{
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
      pSalir->signal_clicked().connect(sigc::ptr_fun(&salir) );
    }
    
    Gtk::Button* pGuardar = nullptr;
    refBuilder->get_widget("Guardar", pGuardar);
    
    if(pGuardar)
    {
      pGuardar->signal_clicked().connect(sigc::ptr_fun(&guardar) );
    }

	Gtk::Button* pRun = nullptr;
	refBuilder->get_widget("Run",pRun);
	model m;
	if(pRun) {
		pRun->signal_clicked().connect(sigc::mem_fun(m,&model::run_model));
	}
 	
	
    app->run(*pMw);
  }

  delete pMw;

  return 0;
}
