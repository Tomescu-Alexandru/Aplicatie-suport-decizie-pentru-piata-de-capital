#include <iostream>
#include <math.h>
#include <fstream>
#include <string>
#include <stdlib.h>
#include <SFML\Graphics.hpp>
using namespace std;


class Buton
{
public:

    float x_poz;
    float y_poz;
    float lungime;
    float latime;
    double x2[16],x1[16];

    Buton(float x, float y, float lung, float lat)
    {
        this->x_poz=x;
        this->y_poz=y;
        this->lungime=lung;
        this->latime=lat;
    }

    void deseneaza(string nume,sf::RenderWindow &window)
    {

        sf::Text text;
        sf::RectangleShape box;

        box.setFillColor(sf::Color::White);
        box.setPosition(x_poz,y_poz);
        box.setSize(sf::Vector2f(lungime,latime));
        box.setOutlineColor(sf::Color::Black);
        box.setFillColor(sf::Color(255,255,204));
        box.setOutlineThickness(3);

        sf::Font font;
        if(!font.loadFromFile("arial.ttf"))
        {
            cout<<"Eroare\n";
        }
        text.setFont(font);
        text.setString(nume);
        text.setCharacterSize(20);
        text.setFillColor(sf::Color::Black);
        text.setPosition(x_poz+5, y_poz);
        window.draw(box);
        window.draw(text);
    }


    bool clicked(sf::Vector2i &vec)
    {
        if((vec.x>=x_poz && vec.x<=(x_poz+lungime)) && (vec.y>=y_poz && vec.y<=(y_poz+latime)))
            return true;
        return false;
    }

};

class Firma
{
public:
    int n;
    int m;
    double A[1236][15];
    int m1;
    double x2[16],x1[16];

    Firma(int n, int m,string nume)
    {
        this->n=n;
        this->m=m;
        m1=m-49;

        ifstream f(nume);
        string sir;
        for (int i=1; i<=m; i++)
            for (int j=1; j<=n; j++)
            {
                if(j<n)
                    getline(f,sir,',');
                else
                    getline(f,sir,'\n');

                A[i][j]=atof(sir.c_str());
            }
         for(int i=0; i<16; i++)
    {
        x1[i]=0;
        x2[i]=0;
    }
    }

    void tort(double a[][16],int m,int n,double u[][16], double *beta)
    {
        int p;
        if(m-1>n)
            p=n;
        else
            p=m-1;


        for(int k=1; k<=p; k++)
        {
            double suma=0;
            for(int i=k; i<=m; i++)
                suma=suma+a[i][k]*a[i][k];
            double sigma=sqrt(suma);
            if(a[k][k]<0)
                sigma=sigma*(-1);

            if(sigma==0)
                beta[k]=0;
            else
            {
                u[k][k]=a[k][k]+sigma;
                for(int i=k+1; i<=m; i++)
                    u[i][k]=a[i][k];
                beta[k]=sigma*u[k][k];
                a[k][k]=(-1)*sigma;

                for(int i=k+1; i<=m; i++)
                    a[i][k]=0;
                for(int j=k+1; j<=n; j++)
                {
                    suma=0;
                    for(int i=k; i<=m; i++)
                        suma=u[i][k]*a[i][j];

                    double r=suma/beta[k];

                    for(int i=k; i<=m; i++)
                        a[i][j]=a[i][j]-r*u[i][k];

                }

            }

        }
    }

    void utris(double u[][16], int n, double *b, double *x)
    {
        for(int i=n; i>=1; i--)
        {

            double s=b[i];
            for(int j=i+1; j<=n; j++)
            {
                s=s-u[i][j]*x[j];

            }

            x[i]=s/u[i][i];
        }
    }

    void cmmp()
    {
        double U[1236][16];
        double beta[16];
        double features[1187][16];
        double b[1187],b2[1187];

        for (int i=1; i<=m1; i++)
            for (int j=1; j<=n; j++)
            {
                if(j==n)
                {
                    features[i][j+1]=1;
                }
                features[i][j]= A[i][j];
                if(j==1 && i>2)
                    b[i-1]= A[i][j];
                if(j==13 && i>2)
                    b2[i-1]=A[i][j];


            }

        tort(features,m1,n+1,U,beta);
        for(int k=1; k<=n; k++)
        {
            double suma=0;
            double suma2=0;
            for(int i=k; i<=m1; i++)
            {
                suma=suma+U[i][k]*b[i];
                suma2=suma2+U[i][k]*b2[i];
            }

            suma=suma/beta[k];
            suma2=suma2/beta[k];
            for(int i=k; i<=m1; i++)
            {
                b[i]=b[i]-suma*U[i][k];
                b2[i]=b2[i]-suma2*U[i][k];

            }

        }
        double R1[16][16],b1[16],R2[16][16],b3[16];
        for(int i=1; i<=n; i++)
            for(int j=1; j<=n; j++)
            {
                R1[i][j]=features[i][j];
                b1[i]=b[i];
                x1[i]=0;
                R2[i][j]=features[i][j];
                b3[i]=b2[i];
                x2[i]=0;

            }


        utris(R1,n,b1,x1);
        utris(R2,n,b3,x2);

    }

    void predictie(double predicted1[],double predicted2[], double x1[], double x2[])
    {
        for(int i=m1; i<=m; i++)
            for (int j=1; j<=n; j++)
            {
                predicted1[i-m1+1] += A[i][j]*x1[j];
                predicted2[i-m1+1] += A[i][j]*x2[j];
                if(j==n)
                {
                    predicted1[i-m1+1] += x1[j+1];
                    predicted2[i-m1+1] += x2[j+1];
                }
            }
    }

};




void plotgraf(string nume, double predicted1[], double A[][15],int n,int m1, float scale2)
{

    float he=650;
    float wi=450;
    float he2=75;
    float wi2=500;
    sf::RenderWindow window(sf::VideoMode(he,wi),nume, sf::Style::Default);
    sf::Vertex Ox[] =
    {
        sf::Vertex(sf::Vector2f(0, wi2-150),sf::Color::Black),
        sf::Vertex(sf::Vector2f(he, wi2-150),sf::Color::Black),
    };
    sf::Vertex Oy[] =
    {
        sf::Vertex(sf::Vector2f(he2, 0),sf::Color::Black),
        sf::Vertex(sf::Vector2f(he2, wi),sf::Color::Black),
    };
    float numar,numar1;
    float punct;
    float scale=10;
    numar1=(float) predicted1[1];
    numar=(float) predicted1[2];
    sf::Vertex linepred[]
    {
        sf::Vertex(sf::Vector2f(1*scale+he2,wi2-numar1*scale2),sf::Color::Blue),
        sf::Vertex(sf::Vector2f(2*scale+he2,wi2-numar*scale2),sf::Color::Blue)
    };
    numar1=(float) A[m1][n];
    numar=(float) A[m1+1][n];
    sf::Vertex linereal[]
    {
        sf::Vertex(sf::Vector2f(1*scale+he2,wi2-numar1*scale2),sf::Color::Red),
        sf::Vertex(sf::Vector2f(2*scale+he2,wi2-numar*scale2),sf::Color::Red)
    };

    sf::Text text;
    sf::Font font;
    if(!font.loadFromFile("arial.ttf"))
    {
        cout<<"Eroare\n";
    }
    text.setFont(font);
    text.setString("1");
    text.setCharacterSize(10);
    text.setFillColor(sf::Color::Black);
    text.move(1*scale+he2, wi2+5-150);

    sf::Vertex liner[]
    {
        sf::Vertex(sf::Vector2f(525,50),sf::Color::Red),
        sf::Vertex(sf::Vector2f(550,50),sf::Color::Red)
    };

    sf::Vertex lineb[]
    {
        sf::Vertex(sf::Vector2f(525,70),sf::Color::Blue),
        sf::Vertex(sf::Vector2f(550,70),sf::Color::Blue)
    };


    sf::Text text3;
    text3.setFont(font);
    text3.setString("Real");
    text3.setCharacterSize(13);
    text3.setFillColor(sf::Color::Black);
    text3.move(555, 40);


    sf::Text text4;
    text4.setFont(font);
    text4.setString("Prezis");
    text4.setCharacterSize(13);
    text4.setFillColor(sf::Color::Black);
    text4.move(555, 60);

    sf::Text text5;
    text5.setFont(font);
    text5.setString("Legenda");
    text5.setCharacterSize(13);
    text5.setFillColor(sf::Color::Black);
    text5.move(531,  20);

    sf::Vertex chenar[]
    {
        sf::Vertex(sf::Vector2f(515,10),sf::Color::Black),
        sf::Vertex(sf::Vector2f(600,10),sf::Color::Black),
        sf::Vertex(sf::Vector2f(600,10),sf::Color::Black),
        sf::Vertex(sf::Vector2f(600,90),sf::Color::Black),
        sf::Vertex(sf::Vector2f(600,90),sf::Color::Black),
        sf::Vertex(sf::Vector2f(515,90),sf::Color::Black),
        sf::Vertex(sf::Vector2f(515,90),sf::Color::Black),
        sf::Vertex(sf::Vector2f(515,10),sf::Color::Black)
    };

    double minim=predicted1[1];
    double maxim=predicted1[1];
    for(int i=2; i<=50; i++)
    {
        if(predicted1[i]<minim)
            minim=predicted1[i];
        if(predicted1[i]>maxim)
            maxim=predicted1[i];
    }
    cout<<"\nMin\Max "<<minim<<" "<<maxim<<endl;
    int maximi=(int) maxim;
    int minimi=(int) minim;
    int upperLimit=maximi+(50-maximi%50);
    int lowerLimit=minimi-minimi%50;
    int diferenta=upperLimit-lowerLimit;

    while(window.isOpen())
    {
        sf::Event even;
        while(window.pollEvent(even))
            if(even.type==even.Closed)
                window.close();


        /// window.draw(line,100,sf::Lines);

        //sf::RectangleShape line2(sf::Vector2f(450, 90));
        // line2.rotate(90);
        //window.draw(line2);
        window.clear(sf::Color::White);
        window.draw(Ox,2,sf::Lines);
        window.draw(Oy,2,sf::Lines);
        window.draw(linepred,2,sf::Lines);
        window.draw(linereal,2,sf::Lines);
        window.draw(liner,2,sf::Lines);
        window.draw(lineb,2,sf::Lines);
        window.draw(chenar,8,sf::Lines);
        window.draw(text3);
        window.draw(text4);
        window.draw(text5);
        window.draw(text);



        int valoare=upperLimit+2*diferenta;
        for(int i=50; i<=300; i+=50)
        {
            sf::Text text2;
            text2.setFont(font);
            text2.setString(to_string(valoare));
            text2.setCharacterSize(10);
            text2.setFillColor(sf::Color::Black);
            text2.setPosition(he2-30, i);
            window.draw(text2);
            valoare-=diferenta;
        }

        for(int i=3; i<=50; i++)
        {
            numar=(float) predicted1[i-1];
            punct=(float) i;
            numar1=(float) predicted1[i];
            sf::Vertex linepred2[]
            {
                sf::Vertex(sf::Vector2f((punct-1)*scale+he2,wi2-numar*scale2),sf::Color::Blue),
                sf::Vertex(sf::Vector2f(punct*scale+he2,wi2-numar1*scale2),sf::Color::Blue)
            };
            window.draw(linepred2,2,sf::Lines);

        }


        for(int i=3; i<=50; i++)
        {
            numar=(float) A[i-2+m1][n];
            punct=(float) i;
            numar1=(float) A[i+m1-1][n];
            sf::Vertex linereal2[]
            {
                sf::Vertex(sf::Vector2f((punct-1)*scale+he2,wi2-numar*scale2),sf::Color::Red),
                sf::Vertex(sf::Vector2f(punct*scale+he2,wi2-numar1*scale2),sf::Color::Red)
            };

            window.draw(linereal2,2,sf::Lines);

            if(i%10==0)
            {
                sf::Text text;
                sf::Font font;
                if(!font.loadFromFile("arial.ttf"))
                {
                    cout<<"Eroare\n";
                }
                text.setFont(font);
                text.setString(to_string(i));
                text.setCharacterSize(10);
                text.setFillColor(sf::Color::Black);
                text.move(punct*scale+he2, wi2+5-150);
                window.draw(text);
            }
        }


        window.display();

    }
}

void apasare_buton(Firma firma1, float scale)
{
    firma1.cmmp();

    double predicted1[51],predicted2[51];
    for(int i=0; i<=51; i++)
    {
        predicted1[i]=0;
        predicted2[i]=0;
    }
    firma1.predictie(predicted1,predicted2,firma1.x1,firma1.x2);

    cout<<"OPEN\n";
    for(int i=1; i<=50; i++)
        cout<<predicted1[i]<<" "<<firma1.A[i+firma1.m1+1][1]<<"\n";

    cout<<"\nCLOSED"<<endl;
    for(int i=1; i<=50; i++)
        cout<<predicted2[i]<<" "<<firma1.A[i+firma1.m1+1][13]<<"\n";

    plotgraf("OPEN",predicted1,firma1.A,1,firma1.m1,scale);
    plotgraf("CLOSED",predicted2,firma1.A,13,firma1.m1,scale);
}

int main()
{

    Firma tata(14,1235,"TATAGlobal.csv");
    Firma bharti(14,492,"hartiar.csv");
    Firma indusind(14,491,"INDUSINDBKALLN.csv");
    Firma cipla(14,491,"CIPLA.csv");
    Firma reliance(14,491,"RELIANCE.csv");





    sf::RenderWindow window1(sf::VideoMode(500,576),"Input", sf::Style::Default);

    Buton buton1= Buton(30,100,120,25);
    Buton buton2= Buton(350,100,115,25);
    Buton buton3= Buton(30,175,140,25);
    Buton buton4= Buton(350,175,120,25);
    Buton buton5= Buton(215,175,90,25);

    sf::Text titlu;
    sf::Font font;
    if(!font.loadFromFile("arial.ttf"))
    {
        cout<<"Eroare\n";
    }
    titlu.setFont(font);
    titlu.setString("Aplicatie suport decizie pentru piata de capital");
    titlu.setCharacterSize(23);
    titlu.setFillColor(sf::Color::Black);
    titlu.setPosition(10,10);

    sf::Texture texture;

    if (!texture.loadFromFile("money1.jpg"))
{
    cout<<"Eroare textura";
}
sf::Sprite sprite;
sprite.setTexture(texture);


    while(window1.isOpen())
    {




        sf::Event even;
        while(window1.pollEvent(even))
            if(even.type==even.Closed)
                window1.close();

        window1.clear();
        window1.draw(sprite);
        buton1.deseneaza("Tata Global",window1);
        buton2.deseneaza("Bharti Airtel",window1);
        buton3.deseneaza("IndusInd Bank",window1);
        buton4.deseneaza("Cipla Limited",window1);
        buton5.deseneaza("Reliance",window1);
        window1.draw(titlu);


        window1.display();


        if (sf::Mouse::isButtonPressed(sf::Mouse::Left))
        {
            sf::Vector2i vec=sf::Mouse::getPosition(window1);

            if(buton1.clicked(vec))
            {
                apasare_buton(tata,2);
            }
            else if(buton2.clicked(vec))
            {
                apasare_buton(bharti,0.75);
            }
            else if(buton3.clicked(vec))
            {
                apasare_buton(indusind,0.22);
            }
            else if(buton4.clicked(vec))
            {
                apasare_buton(cipla,0.70);
            }
            else if(buton5.clicked(vec))
            {
                apasare_buton(reliance,0.21);
            }
        }
    }



    return 0;

}
