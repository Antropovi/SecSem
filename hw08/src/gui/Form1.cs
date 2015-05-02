using System;
using System.Windows.Forms;
using FSharp;


namespace gui
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button13_Click(object sender, EventArgs e)
        {

            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("*");

        }

        private void button12_Click(object sender, EventArgs e)
        {
            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("/");

        }

        private void button3_Click(object sender, EventArgs e)
        {
            Calc.accVal("9");
            label1.Text = Calc.returnVal();
        }

        private void button7_Click(object sender, EventArgs e)
        {
            Calc.accVal("1");
            label1.Text = Calc.returnVal();
        }

        private void label1_Click(object sender, EventArgs e)
        {
            label1.Text = Calc.returnVal();
        }

        private void button8_Click(object sender, EventArgs e)
        {
            Calc.accVal("2");
            label1.Text = Calc.returnVal();
        }

        private void button9_Click(object sender, EventArgs e)
        {
            Calc.accVal("3");
            label1.Text = Calc.returnVal();
        }

        private void button4_Click(object sender, EventArgs e)
        {
            Calc.accVal("4");
            label1.Text = Calc.returnVal();
        }

        private void button5_Click(object sender, EventArgs e)
        {
            Calc.accVal("5");
            label1.Text = Calc.returnVal();
        }

        private void button6_Click(object sender, EventArgs e)
        {
            Calc.accVal("6");
            label1.Text = Calc.returnVal();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Calc.accVal("7");
            label1.Text = Calc.returnVal();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Calc.accVal("8");
            label1.Text = Calc.returnVal();
        }

        private void button10_Click(object sender, EventArgs e)
        {
            Calc.accVal("0");
            label1.Text = Calc.returnVal();
        }

        private void button15_Click(object sender, EventArgs e)
        {
            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("+");
        }

        private void button16_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.setFlag();
            label1.Text = Calc.returnVal();
        }

        private void button14_Click(object sender, EventArgs e)
        {
            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("-");

        }

        private void button18_Click(object sender, EventArgs e)
        {
            Calc.clearAll();
            label1.Text = "0";
        }

        private void button17_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("+/-");
            label1.Text = Calc.returnVal();

        }

        private void button19_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("sin");
            label1.Text = Calc.returnVal();
        }

        private void button22_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("cos");
            label1.Text = Calc.returnVal();
        }

        private void button25_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("tan");
            label1.Text = Calc.returnVal();
        }

        private void button20_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("^2");
            label1.Text = Calc.returnVal();
        }

        private void button23_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("^3");
            label1.Text = Calc.returnVal();
        }

        private void button26_Click(object sender, EventArgs e)
        {
            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("^n");
        }

        private void button11_Click(object sender, EventArgs e)
        {
            Calc.accVal(",");
            label1.Text = Calc.returnVal();

        }

        private void button27_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("10^x");
            label1.Text = Calc.returnVal();
        }

        private void button30_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("asin");
            label1.Text = Calc.returnVal();
        }

        private void button29_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("acos");
            label1.Text = Calc.returnVal();
        }

        private void button28_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("atan");
            label1.Text = Calc.returnVal();
        }

        private void button21_Click(object sender, EventArgs e)
        {
            Calc.equal();
            Calc.unaryOper("ln");
            label1.Text = Calc.returnVal();
        }

        private void button24_Click(object sender, EventArgs e)
        {
            Calc.equal();
            label1.Text = Calc.returnVal();
            Calc.operSet("log");
        }

        private void button31_Click(object sender, EventArgs e)
        {
            chart1.Series[0].Points.Clear();
            double temp1 = Convert.ToDouble(textBox2.Text);
            double temp2 = Convert.ToDouble(textBox3.Text);
            double temp3 = Convert.ToDouble(textBox4.Text);

            for (double i = temp1; i <= temp2; i+=temp3 )
            {
                double Y = Graph.calc(textBox1.Text, i);
                chart1.Series[0].Points.AddXY(i, Y);
            }
            chart1.DataBind();
        }
    }
}
