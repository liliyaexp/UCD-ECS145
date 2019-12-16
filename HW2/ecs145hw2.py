import threading
import os
import Queue

class walkThread(threading.Thread):
    # initialize work queue
    workq = Queue.Queue()
    thrdlist = []
    result = None
    # use to lock result when modifying result
    resultlock = threading.Lock()

    def __init__(self, id):
        threading.Thread.__init__(self)
        self.myid = id

    # overload run method
    def run(self):
        if walkThread.useHidden:
            while (1):
                try:
                    # set 0.1 second for timeout
                    full_path = walkThread.workq.get(True, 0.1)
                except:
                    break
                name_split = full_path.split("/")
                file_name = name_split[-1]
                if os.path.isdir(full_path):
                    # run user function
                    walkThread.resultlock.acquire()
                    walkThread.nodeftn(file_name)
                    walkThread.resultlock.release()
                    file_list = os.listdir(full_path)
                    for fl in file_list:
                        # store absolute path in work queue
                        walkThread.workq.put(os.path.join(full_path, fl))
                else:
                    # run user function
                    walkThread.resultlock.acquire()
                    walkThread.nodeftn(file_name)
                    walkThread.resultlock.release()
        else:
            while (1):
                try:
                    full_path = walkThread.workq.get(True, 0.1)
                except:
                    break
                name_split = full_path.split("/")
                file_name = name_split[-1]
                if file_name[0] == ".":
                    continue
                if os.path.isdir(full_path):
                    # run user function
                    walkThread.resultlock.acquire()
                    walkThread.nodeftn(file_name)
                    walkThread.resultlock.release()
                    file_list = os.listdir(full_path)
                    for fl in file_list:
                        # store absolute path in work queue
                        walkThread.workq.put(os.path.join(full_path, fl))
                else:
                    # run user function
                    walkThread.resultlock.acquire()
                    walkThread.nodeftn(file_name)
                    walkThread.resultlock.release()

def tWalk(startdir, fileftn, nthreads, useHiddenFiles=True):
    # Dynamically add class method
    walkThread.nodeftn = classmethod(fileftn)
    startdir = os.path.abspath(startdir)
    walkThread.workq.put(startdir)
    walkThread.useHidden = useHiddenFiles
    # start thread
    for i in range(nthreads):
        fn = walkThread(i)
        walkThread.thrdlist.append(fn)
        fn.start()
    # waiting to end the program
    for thrd in walkThread.thrdlist: thrd.join()