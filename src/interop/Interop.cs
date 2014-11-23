using System;
using System.Collections;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using Fingertrees;

namespace Interop
{
    class Program
    {
        static void Main(string[] args)
        {
            var strings = new List<string>
                {
                 "bb", "ffffff", "a", "ccc", "eeeee", "dddd"
                };
            var fsharpList = ListModule.OfSeq(strings);
            var ft = PriorityQueue.listToPriority(fsharpList);
            Console.WriteLine(String.Format("Finger Tree: {0}", ft));
        }
    }
}