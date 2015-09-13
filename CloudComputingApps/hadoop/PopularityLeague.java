import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.io.ArrayWritable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.KeyValueTextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

import java.io.*;
import java.util.*;


public class PopularityLeague extends Configured implements Tool {

    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new PopularityLeague(), args);
        System.exit(res);
    }

    public static class IntArrayWritable extends ArrayWritable {
        public IntArrayWritable() {
            super(IntWritable.class);
        }

        public IntArrayWritable(Integer[] numbers) {
            super(IntWritable.class);
            IntWritable[] ints = new IntWritable[numbers.length];
            for (int i = 0; i < numbers.length; i++) {
                ints[i] = new IntWritable(numbers[i]);
            }
            set(ints);
        }
    }

    @Override
    public int run(String[] args) throws Exception {        
        Configuration conf = this.getConf();
        FileSystem fs = FileSystem.get(conf);
        Path tmpPath = new Path("/mp2/tmp");
        fs.delete(tmpPath, true);

        Job jobA = Job.getInstance(conf, "Link Count");
        jobA.setOutputKeyClass(IntWritable.class);
        jobA.setOutputValueClass(IntWritable.class);

        jobA.setMapperClass(LinkCountMap.class);
        jobA.setReducerClass(LinkCountReduce.class);

        FileInputFormat.setInputPaths(jobA, new Path(args[0]));
        FileOutputFormat.setOutputPath(jobA, tmpPath);

        jobA.setJarByClass(PopularityLeague.class);
        jobA.waitForCompletion(true);

        Job jobB = Job.getInstance(conf, "Popularity");
        jobB.setOutputKeyClass(IntWritable.class);
        jobB.setOutputValueClass(IntWritable.class);

        jobB.setMapOutputKeyClass(NullWritable.class);
        jobB.setMapOutputValueClass(IntArrayWritable.class);

        jobB.setMapperClass(PopularityMap.class);
        jobB.setReducerClass(PopularityReduce.class);
        jobB.setNumReduceTasks(1);

        FileInputFormat.setInputPaths(jobB, tmpPath);
        FileOutputFormat.setOutputPath(jobB, new Path(args[1]));

        jobB.setJarByClass(PopularityLeague.class);
        return jobB.waitForCompletion(true) ? 0 : 1;
    }

    public static String readHDFSFile(String path, Configuration conf) throws IOException{
        Path pt=new Path(path);
        FileSystem fs = FileSystem.get(pt.toUri(), conf);
        FSDataInputStream file = fs.open(pt);
        BufferedReader buffIn=new BufferedReader(new InputStreamReader(file));

        StringBuilder everything = new StringBuilder();
        String line;
        while( (line = buffIn.readLine()) != null) {
            everything.append(line);
            everything.append("\n");
        }
        return everything.toString();
    }

    public static class LinkCountMap extends Mapper<Object, Text, IntWritable, IntWritable> {
        @Override
        public void map(Object key, Text value, Context context) throws IOException, InterruptedException {
            String line = value.toString();
            StringTokenizer tokenizer = new StringTokenizer(line);
            while (tokenizer.hasMoreTokens()) {
                String nextToken = tokenizer.nextToken();
                if (nextToken.charAt(nextToken.length()-1) == ':') {
                    context.write(new IntWritable(Integer.parseInt(nextToken.substring(0,nextToken.length()-1))), new IntWritable(0));
                } else {
                    context.write(new IntWritable(Integer.parseInt(nextToken)), new IntWritable(1));
                }
            }
        }
    }

    public static class LinkCountReduce extends Reducer<IntWritable, IntWritable, IntWritable, IntWritable> {
        @Override
        public void reduce(IntWritable key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
            int sum = 0;
            for (IntWritable val : values) {
                sum += val.get();
            }
            context.write(new IntWritable(key.get()), new IntWritable(sum));
        }
    }

    public static class PopularityMap extends Mapper<Object, Text, NullWritable, IntArrayWritable> {
        Set<Integer> league;
        ArrayList<Pair<IntWritable,IntWritable>> collect = new ArrayList<Pair<IntWritable,IntWritable>>();


        @Override
        protected void setup(Context context) throws IOException,InterruptedException {
            Configuration conf = context.getConfiguration();

            String leaguePath = conf.get("league");
            List<String> ls = Arrays.asList(readHDFSFile(leaguePath, conf).split("\n"));
            league = new TreeSet<Integer>();
            for (String l: ls) league.add(Integer.parseInt(l));
        }
        
        @Override
        public void map(Object key, Text value, Context context) throws IOException, InterruptedException {
            String[] keyVal = value.toString().split("[ \t]");
            int k = Integer.parseInt(keyVal[0]);
            int v = Integer.parseInt(keyVal[1]);
            if (league.contains(k))
            	collect.add(new Pair<IntWritable,IntWritable>(new IntWritable(v),new IntWritable(k)));
        }

        @Override
        @SuppressWarnings("unchecked")
        protected void cleanup(Context context) throws IOException, InterruptedException {
            Collections.sort(collect);
            Object[] pairs = collect.toArray();
            Integer[] ints = new Integer[2*pairs.length];
            for (int i = 0 ; i < pairs.length ; i++ ) {
                ints[2*i] = ((Pair<IntWritable,IntWritable>)pairs[i]).first.get();
                ints[2*i+1] = ((Pair<IntWritable,IntWritable>)pairs[i]).second.get();
            }
            context.write(NullWritable.get(), new IntArrayWritable(ints));

            Configuration conf = context.getConfiguration();
            FileSystem fs = FileSystem.get(conf);
            Path tmpPath = new Path("/mp2/tmp");
            fs.delete(tmpPath, true);
        }
    }

    public static class PopularityReduce extends Reducer<NullWritable, IntArrayWritable, IntWritable, IntWritable> {
        ArrayList<Pair<IntWritable,Text>> collect = new ArrayList<Pair<IntWritable,Text>>();

        @Override
        protected void setup(Context context) throws IOException,InterruptedException {
            Configuration conf = context.getConfiguration();
        }

        @Override
        @SuppressWarnings("unchecked")
        public void reduce(NullWritable key, Iterable<IntArrayWritable> values, Context context) throws IOException, InterruptedException {
            for (IntArrayWritable ia : values)
                for (int i = 0; i < ia.get().length ; i += 2) {
                    collect.add(new Pair(new IntWritable(((IntWritable)ia.get()[i]).get()), new IntWritable(((IntWritable)ia.get()[i+1]).get())));
                }
            Collections.sort(collect);
            Object[] pairs = collect.toArray();
            ArrayList<Pair<IntWritable,IntWritable>> ranks = new ArrayList<Pair<IntWritable,IntWritable>>();
            int lastPop = 0;
            int lastRank = 0;
            int i = 0;
            for ( Object o: pairs ) {
                Pair<IntWritable,IntWritable> p = (Pair<IntWritable,IntWritable>)o ;
                int link = p.second.get();
                int pop = p.first.get();
                if (pop != lastPop) {
                	lastPop = pop;
                	lastRank = i;
                }
                ranks.add(new Pair<IntWritable,IntWritable>(new IntWritable(link),new IntWritable(lastRank)));
                i ++ ;
            }

            for ( Pair<IntWritable,IntWritable> p: ranks ) 
                context.write(p.first,p.second);
        }        
    }
}

class Pair<A extends Comparable<? super A>,
        B extends Comparable<? super B>>
        implements Comparable<Pair<A, B>> {

    public final A first;
    public final B second;

    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    public static <A extends Comparable<? super A>,
            B extends Comparable<? super B>>
    Pair<A, B> of(A first, B second) {
        return new Pair<A, B>(first, second);
    }

    @Override
    public int compareTo(Pair<A, B> o) {
        int cmp = o == null ? 1 : (this.first).compareTo(o.first);
        return cmp == 0 ? (this.second).compareTo(o.second) : cmp;
    }

    @Override
    public int hashCode() {
        return 31 * hashcode(first) + hashcode(second);
    }

    private static int hashcode(Object o) {
        return o == null ? 0 : o.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Pair))
            return false;
        if (this == obj)
            return true;
        return equal(first, ((Pair<?, ?>) obj).first)
                && equal(second, ((Pair<?, ?>) obj).second);
    }

    private boolean equal(Object o1, Object o2) {
        return o1 == o2 || (o1 != null && o1.equals(o2));
    }

    @Override
    public String toString() {
        return "(" + first + ", " + second + ')';
    }
}
