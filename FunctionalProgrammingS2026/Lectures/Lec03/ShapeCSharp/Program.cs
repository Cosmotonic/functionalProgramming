using System;

namespace ShapeCSharp
{
    public abstract class Shape {
        abstract public double area();
    }
    
    public class Circle : Shape {
        private double _r;

        public Circle(double r) {
            _r = r;
        }

        public override double area() {
            return System.Math.PI * _r * _r;
        }
    }

    public class Square : Shape {
        private double _a;

        public Square(double a) {
            _a = a;
        }

        public override double area() {
            return _a * _a;
        }

    }   
    public class Triangle : Shape {
        private double _a,_b,_c;

        public Triangle(double a,double b,double c) {
            _a = a;
            _b = b;
            _c = c;
        }

        public override double area() {
            double s = (_a+_b+_c)/2.0;
            return System.Math.Sqrt(s*(s-_a)*(s-_b)*(s-_c));
        }

    }   

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Shape Example");

            Shape circle = new Circle(2.0);
            Shape triangle = new Triangle(1.0, 2.0, 3.0);
            Shape square = new Square(4.0);

            Console.WriteLine("Area of circle=" + circle.area().ToString());
            Console.WriteLine("Area of triangle=" + triangle.area().ToString()); 
            Console.WriteLine("Area of square=" + square.area().ToString());

        }
    }
}
