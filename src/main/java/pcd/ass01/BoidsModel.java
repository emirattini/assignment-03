package pcd.ass01;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class BoidsModel {
    
    private List<Boid> boids;
    private double separationWeight; 
    private double alignmentWeight; 
    private double cohesionWeight; 
    private final double width;
    private final double height;
    private final double maxSpeed;
    private final double perceptionRadius;
    private final double avoidRadius;
    private volatile boolean isRunning;
    private volatile boolean isSuspended;
    private int nBoids;
    private final Lock separationWeightLock;
    private final Lock alignmentWeightLock;
    private final Lock cohesionWeightLock;

    private final int maxFramerate;
    private int framerate;
    private long startingTime;

    public BoidsModel(int nBoids,
                            double initialSeparationWeight,
    						double initialAlignmentWeight,
    						double initialCohesionWeight,
    						double width,
    						double height,
    						double maxSpeed,
    						double perceptionRadius,
    						double avoidRadius,
                            int maxFramerate){
        this.separationWeight = initialSeparationWeight;
        this.alignmentWeight = initialAlignmentWeight;
        this.cohesionWeight = initialCohesionWeight;
        this.width = width;
        this.height = height;
        this.maxSpeed = maxSpeed;
        this.perceptionRadius = perceptionRadius;
        this.avoidRadius = avoidRadius;
        this.boids = new ArrayList<>();
        this.nBoids = nBoids;
        this.isRunning = false;
        this.isSuspended = false;
        this.separationWeightLock = new ReentrantLock();
        this.alignmentWeightLock = new ReentrantLock();
        this.cohesionWeightLock = new ReentrantLock();
        this.maxFramerate = maxFramerate;
    }

    public boolean isRunning() {
        return isRunning;
    }

    public void turnOn() {
        isRunning = true;
    }

    public void turnOff() {
        isRunning = false;
    }

    public boolean isSuspended() {
        return isSuspended;
    }

    public void suspend() {
        this.isSuspended = true;
    }

    public void resume() {
        this.isSuspended = false;
    }

    public void generateBoids() {
        for (int i = 0; i < nBoids; i++) {
            P2d pos = new P2d(-width /2 + Math.random() * width, -height /2 + Math.random() * height);
            V2d vel = new V2d(Math.random() * maxSpeed /2 - maxSpeed /4, Math.random() * maxSpeed /2 - maxSpeed /4);
            this.boids.add(new Boid(pos, vel));
        }
    }

    public void clearBoids() {
        this.nBoids = 0;
        this.boids = new ArrayList<>();
    }
    
    public List<Boid> getBoids(){
    	return boids;
    }

    public void setBoids(Collection<Boid> boids) {
        this.boids = new ArrayList<>(boids);
    }

    public double getMinX() {
    	return -width/2;
    }

    public double getMaxX() {
    	return width/2;
    }

    public double getMinY() {
    	return -height/2;
    }

    public double getMaxY() {
    	return height/2;
    }
    
    public double getWidth() {
    	return width;
    }
 
    public double getHeight() {
    	return height;
    }

    public void setSeparationWeight(double value) {
        this.separationWeightLock.lock();
        this.separationWeight = value;
        this.separationWeightLock.unlock();
    }

    public void setAlignmentWeight(double value) {
        this.alignmentWeightLock.lock();
        this.alignmentWeight = value;
        this.alignmentWeightLock.unlock();
    }

    public void setCohesionWeight(double value) {
        this.cohesionWeightLock.lock();
        this.cohesionWeight = value;
        this.cohesionWeightLock.unlock();
    }

    public double getSeparationWeight() {
    	return separationWeight;
    }

    public double getCohesionWeight() {
    	return cohesionWeight;
    }

    public double getAlignmentWeight() {
    	return alignmentWeight;
    }
    
    public double getMaxSpeed() {
    	return maxSpeed;
    }

    public double getAvoidRadius() {
    	return avoidRadius;
    }

    public double getPerceptionRadius() {
    	return perceptionRadius;
    }

    public void setBoidsNumber(int n) {
        this.nBoids = n;
    }

    public int getMaxFramerate() {
        return maxFramerate;
    }

    public void setStartingTime(long startingTime) {
        this.startingTime = startingTime;
    }

    public long getStartingTime() {
        return startingTime;
    }

    public long getElapsedTime() {
        return Math.abs(this.startingTime - System.currentTimeMillis());
    }

    public void setFramerate(int framerate) {
        this.framerate = framerate;
    }

    public int getFramerate() {
        return this.framerate;
    }
}
